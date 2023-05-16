package equity

import cats._
import cats.syntax.all._
import cats.data.{NonEmptyList, State}
import cats.effect.IO
import cats.effect.std.Random
import cats.implicits.catsSyntaxApplicativeId
import equity.EquityService.{equityFrom, getOrDeal, winners, SimState}
import poker.OrderInstances.handOrder
import poker.{Card, Hand, Position, Rank, Suit}

final case class SimSetup[F[_]] private (
  // Do I even need a deck? The deck basically gets derived
  // from the cards that are prepopulated by the sim
//  deck: F[SimDeck],
  players: List[
    SimPlayer[F]
  ],
  // TODO Should I type these to make subsequent code more expressive?
  card1: F[Card],
  card2: F[Card],
  card3: F[Card],
  turn: F[Card],
  river: F[Card]
) {

//  def mapK[G[_]](f: F[Card] => G[Card]): SimSetup[G] = ???

}

object SimSetup {
//
//  def applyK(
//    players: List[SimPlayer[Option]],
//    card1: Option[Card],
//    card2: Option[Card],
//    card3: Option[Card],
//    turn: Option[Card],
//    river: Option[Card]
//  ): SimSetup[Id] = {
//
//  }
}

final case class SimPlayer[F[_]] private (
  position: Position,
  card1: F[Card],
  card2: F[Card]
) {

  // TODO not sure how to use this yet.
//  def mapK[G[_]](f: F[Card] => G[Card]): SimPlayer[G] =
//    copy(card1 = f(card1), card2 = f(card2))  ???

}
object SimPlayer {
  // TODO refactor??
  // This is so strange... why would a player method return a deck???
  def applyK(
    position: Position,
    card1: Option[Card],
    card2: Option[Card]
  )(deck: SimDeck): (SimPlayer[Option], SimDeck) = {

    val getOrDealPlayerCards = for {
      cardA <- getOrDeal(card1)
      cardB <- getOrDeal(card2)
    } yield SimPlayer(position, cardA.some, cardB.some)
    getOrDealPlayerCards.run(deck).value.swap
  }
}

final case class SimDeck(cards: NonEmptyList[Card]) {

  // TODO this isnt being used yet.
  def shuffle[F[_]: Functor: Random]: F[List[Card]] =
    Random[F].shuffleList(cards.toList)

  private def startingDeckImpl: SimDeck = {
    val cards = for {
      rank <- Rank.all
      suit <- Suit.all
    } yield Card(rank, suit)
    SimDeck(NonEmptyList.fromList(cards).get)
  }

}

final case class SimResult(winnersList: List[Position]) {}

final case class EquityCalculation(equities: Map[Position, List[Double]])

sealed trait EquityService[F[_], G[_]] {

  type SimState[A] = State[(SimSetup[G], SimDeck), A]

  type SimDeckState[A] = State[SimDeck, A]

  def deckFrom: SimSetup[G] => SimDeck => SimDeck

  def hydratedSim: SimState[EquityCalculation]

  def runThis(simSetupOp: SimSetup[G])(simDeck: SimDeck): EquityCalculation

  def equityFrom(result: SimResult): EquityCalculation

  def combinedEquity(
    eq1: EquityCalculation,
    eq2: EquityCalculation
  ): EquityCalculation

  def theFinalEquityOf[F[_]: Functor: Random](
    sim: SimSetup[G],
    n: Int
  )(simDeck: F[SimDeck]): F[EquityCalculation]

  def aggregateEquityFromMany(
    results: List[EquityCalculation]
  ): EquityCalculation =
    results
      .foldLeft(
        EquityCalculation(Map.empty[Position, List[Double]])
      ) { (s, v) =>
        combinedEquity(s, v)
      }
}

object EquityService extends EquityService[IO, Option] {

  // TODO Another unsafe spot I don't know how to deal with.
  val dealOneCard: SimDeckState[Card] = State[SimDeck, Card] {
    _.cards match {
      case NonEmptyList(card, remaining) =>
        (SimDeck(NonEmptyList.fromList(remaining).get), card)
    }

  }

  val getOrDeal: Option[Card] => SimDeckState[Card] = optCard =>
    for {
      newCard <- optCard match {
        case Some(card) => State.pure[SimDeck, Card](card)
        case None       => dealOneCard
      }
    } yield newCard

  override def deckFrom: SimSetup[Option] => SimDeck => SimDeck = {

    // TODO What is a better way to do this?
    def allCardsFrom(simSetup: SimSetup[Option]): List[Card] = {
      val boardCards = simSetup match {
        case SimSetup(_, c1, c2, c3, t, r) =>
          List(c1, c2, c3, t, r).flatMap(_.fold(List.empty[Card])(List(_)))
      }

      // Player cards
      val cardsFrom: SimPlayer[Option] => List[Card] = {
        case SimPlayer(_, c1, c2) =>
          List(c1, c2).flatMap(_.fold(List.empty[Card])(List(_)))
      }

      def allPlayerCards = simSetup.players.foldLeft(List.empty[Card]) {
        (cards, player) =>
          cards ++ cardsFrom(player)
      }

      boardCards ++ allPlayerCards
    }

    sim: SimSetup[Option] =>
      simDeck: SimDeck =>
        val finalDeck = simDeck.cards
          .filterNot(allCardsFrom(sim).contains(_))
          .pure[Id]
        SimDeck(NonEmptyList.fromList(finalDeck).get)
  }

  def winners(l: List[(Position, Hand)]): SimResult =
    SimResult(
      l.maximumByList[Hand](x => x._2)
        .map { case (playerPosition, _) => playerPosition }
    )

  def preFlopEquity(sim: SimSetup[Option]): EquityCalculation = {
    val simResult: SimResult = winners(
      PreFlop
        .allHands(sim)
    )
    equityFrom(simResult)
  }

  private def hydratePlayer(player: SimPlayer[Option])(
    deck: SimDeck
  ): (SimPlayer[Option], SimDeck) =
    SimPlayer.applyK(player.position, player.card1, player.card2)(deck)

  private val stateAfterPlayers: SimState[EquityCalculation] =
    State[(SimSetup[Option], SimDeck), EquityCalculation] { case (sim, deck) =>
      val (allNewPlayers, finalDeck) = sim.players.foldLeft(
        (List.empty[SimPlayer[Option]], deck)
      ) { case ((allPlayers, deck), player) =>
        val (newPlayer, newDeck) = hydratePlayer(player)(deck)
        (allPlayers :+ newPlayer, newDeck)
      }

      val newSim = sim.copy(players = allNewPlayers)

      (
        (newSim, finalDeck),
        preFlopEquity(newSim)
      )
    }

  override def hydratedSim: SimState[EquityCalculation] =
    for {
      _           <- stateAfterPlayers
      _           <- SimBoardState.after(FlopCard1)
      _           <- SimBoardState.after(FlopCard2)
      _           <- SimBoardState.after(FlopCard3)
      _           <- SimBoardState.after(Turn)
      finalEquity <- SimBoardState.after(River)
    } yield finalEquity

  override def runThis(
    simSetupOp: SimSetup[Option]
  )(simDeck: SimDeck): EquityCalculation = {
    val deck = deckFrom(simSetupOp)(simDeck)
    hydratedSim.runA(simSetupOp, deck).value
  }

  override def equityFrom(result: SimResult): EquityCalculation = {
    val divisor = result.winnersList.size
    EquityCalculation(result.winnersList.map(_ -> List(100.0 / divisor)).toMap)
  }

  override def combinedEquity(
    eq1: EquityCalculation,
    eq2: EquityCalculation
  ): EquityCalculation =
    EquityCalculation(
      eq1.equities ++ eq2.equities
        .map { case (k, v) =>
          k -> (v ++ eq1.equities.getOrElse(k, List.empty))
        }
    )

  // TODO This is where we return an IO right?
  // the deck injected should be pre shuffled, I think.

  override def theFinalEquityOf[F[_]: Functor: Random](
    sim: SimSetup[Option],
    n: Int
  )(deck: F[SimDeck]): F[EquityCalculation] =
    for {
      shuffled <- deck
      simResults = (1 to n).toList.map(_ => runThis(sim)(shuffled))
    } yield aggregateEquityFromMany(simResults)
}

// TODO Organization - what should be in Equity Service vs SimBoardState
sealed trait SimBoardState {
  def allHands(sim: SimSetup[Option]): List[(Position, Hand)] = {
    val boardCards: List[Card] = sim match {
      case SimSetup(_, card1, card2, card3, turn, river) =>
        List(card1, card2, card3, turn, river).flatMap(
          _.fold(List.empty[Card])(List(_))
        )
    }
    for {
      player <- sim.players
    } yield (
      player.position,
      Hand.rank(player.card1.get :: player.card2.get :: boardCards)
    )
  }
}

object SimBoardState {

  def after(state: SimBoardState): SimState[EquityCalculation] =
    State[(SimSetup[Option], SimDeck), EquityCalculation] { case (sim, deck) =>
      val (newDeck, newCard) = state match {
        // TODO this is wonky but kinda acts as a "burn" card...
        // Must change this.
        case PreFlop   => getOrDeal(Option.empty[Card]).run(deck).value
        case FlopCard1 => getOrDeal(sim.card1).run(deck).value
        case FlopCard2 => getOrDeal(sim.card2).run(deck).value
        case FlopCard3 => getOrDeal(sim.card3).run(deck).value
        case Turn      => getOrDeal(sim.turn).run(deck).value
        case River     => getOrDeal(sim.river).run(deck).value
      }

      val newSim = state match {
        case PreFlop   => sim
        case FlopCard1 => sim.copy(card1 = newCard.pure[Option])
        case FlopCard2 => sim.copy(card2 = newCard.pure[Option])
        case FlopCard3 => sim.copy(card3 = newCard.pure[Option])
        case Turn      => sim.copy(turn = newCard.pure[Option])
        case River     => sim.copy(river = newCard.pure[Option])
      }

      val equityCalculation = equityFrom(winners(state.allHands(newSim)))

      ((newSim, newDeck), equityCalculation)
    }
}

final case object PreFlop   extends SimBoardState
final case object FlopCard1 extends SimBoardState
final case object FlopCard2 extends SimBoardState
final case object FlopCard3 extends SimBoardState
final case object Turn      extends SimBoardState
final case object River     extends SimBoardState
