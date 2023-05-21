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

// TODO Get rid of the Sim prefix, put this in a sim package!

final case class SimSetup private (
  // Do I even need a deck? The deck basically gets derived
  // from the cards that are prepopulated by the sim
//  deck: F[SimDeck],
  players: List[
    SimPlayer
  ],
  // TODO Should I type these to make subsequent code more expressive?
  card1: Option[Card],
  card2: Option[Card],
  card3: Option[Card],
  turn: Option[Card],
  river: Option[Card]
) {

//  def mapK[G[_]](f: F[Card] => G[Card]): SimSetup[G] = ???

}

object SimSetup {

  // TODO There needs to be validation on the the SimSetup - in other words
  // you shouldn't be able to have an illegal SimSetup
  // Every card should be distinct and there should be at least 2 players.

  def validate[F[_]](simDeck: SimSetup): F[Unit] = ???
//
//  def applyK(
//    players: List[SimPlayer],
//    card1: Option[Card],
//    card2: Option[Card],
//    card3: Option[Card],
//    turn: Option[Card],
//    river: Option[Card]
//  ): SimSetup[Id] = {
//
//  }
}

final case class SimPlayer private (
  position: Position,
  card1: Option[Card],
  card2: Option[Card]
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
  )(deck: SimDeck): (SimPlayer, SimDeck) = {

    val getOrDealPlayerCards = for {
      cardA <- getOrDeal(card1)
      cardB <- getOrDeal(card2)
    } yield SimPlayer(position, cardA.some, cardB.some)
    getOrDealPlayerCards.run(deck).value.swap
  }
}

final case class SimDeck(cards: List[Card]) {

  // TODO this isnt being used yet.
  def shuffle[F[_]: Functor: Random]: F[SimDeck] =
    Random[F].shuffleList(cards).map(SimDeck)

  private def startingDeckImpl: SimDeck = {
    val cards = for {
      rank <- Rank.all
      suit <- Suit.all
    } yield Card(rank, suit)
    SimDeck(cards)
  }

}

final case class SimResult(winnersList: List[Position]) {}

// TODO Name: Positional Equities?
final case class EquityCalculation(equities: Map[Position, List[Double]])

sealed trait EquityService[F[_]] {

  type SimState[A] = State[(SimSetup, SimDeck), A]

  type SimDeckState[A] = State[SimDeck, A]

//  def deckFrom: SimSetup[G] => SimDeck => SimDeck
// TODO This other functions are really implementation, they
  // don't belong in the algebra
  def hydratedSim: SimState[EquityCalculation]

  def runThis(simSetupOp: SimSetup)(simDeck: SimDeck): EquityCalculation

  def equityFrom(result: SimResult): EquityCalculation

  def combinedEquity(
    eq1: EquityCalculation,
    eq2: EquityCalculation
  ): EquityCalculation

  def theFinalEquityOf[F[_]: Functor: Random: Applicative](
    sim: SimSetup,
    n: Int
  )(simDeck: SimDeck): F[EquityCalculation]

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

object EquityService extends EquityService[IO] {

  val dealOneCard: SimDeckState[Card] = State[SimDeck, Card] {
    _.cards match {
      case card :: remaining => (SimDeck(remaining), card)
      case _                 => throw new Exception("BUG: No Cards in Deck!!!")
    }
  }

  val getOrDeal: Option[Card] => SimDeckState[Card] = optCard =>
    for {
      newCard <- optCard match {
        case Some(card) => State.pure[SimDeck, Card](card)
        case None       => dealOneCard
      }
    } yield newCard

  def deckFrom: SimSetup => SimDeck => SimDeck = {

    def allCardsFrom(simSetup: SimSetup): List[Card] = {
      val boardCards = simSetup match {
        case SimSetup(_, c1, c2, c3, t, r) =>
          List(c1, c2, c3, t, r).collect { case Some(x) => x }
      }

      // Player cards
      val cardsFrom: SimPlayer => List[Card] = { case SimPlayer(_, c1, c2) =>
        List(c1, c2).collect { case Some(x) => x }
      }

      def allPlayerCards = simSetup.players.foldLeft(List.empty[Card]) {
        (cards, player) =>
          cards ++ cardsFrom(player)
      }

      boardCards ++ allPlayerCards
    }

    sim: SimSetup =>
      simDeck: SimDeck =>
        val finalDeck = simDeck.cards
          .filterNot(allCardsFrom(sim).contains(_))
          .pure[Id]
        SimDeck(finalDeck)
  }

  def winners(l: List[(Position, Hand)]): SimResult =
    SimResult(
      l.maximumByList[Hand](x => x._2)
        .map { case (playerPosition, _) => playerPosition }
    )

  def preFlopEquity(sim: SimSetup): EquityCalculation = {
    val simResult: SimResult = winners(
      PreFlop
        .allHands(sim)
    )
    equityFrom(simResult)
  }

  private def hydratePlayer(player: SimPlayer)(
    deck: SimDeck
  ): (SimPlayer, SimDeck) =
    SimPlayer.applyK(player.position, player.card1, player.card2)(deck)

  private val stateAfterPreFlopCards: SimState[EquityCalculation] =
    State[(SimSetup, SimDeck), EquityCalculation] { case (sim, deck) =>
      val (allNewPlayers, finalDeck) = sim.players.foldLeft(
        (List.empty[SimPlayer], deck)
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

  def after(state: SimBoardState): SimState[EquityCalculation] =
    State[(SimSetup, SimDeck), EquityCalculation] { case (sim, deck) =>
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

  // TODO These after calls can actually be distinct functions.
  // TODO I don't really need my state's result to be EquityCalculation.  It can be Unit.
  override def hydratedSim: SimState[EquityCalculation] =
    for {
      _           <- stateAfterPreFlopCards
      _           <- after(FlopCard1)
      _           <- after(FlopCard2)
      _           <- after(FlopCard3)
      _           <- after(Turn)
      finalEquity <- after(River)
    } yield finalEquity

  override def runThis(
    simSetupOp: SimSetup
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

  override def theFinalEquityOf[F[_]: Functor: Random: Applicative](
    sim: SimSetup,
    n: Int
  )(deck: SimDeck): F[EquityCalculation] =
    // TODO The Unit result means I did NOT short circuit
    for {
//      _             <- SimSetup.validate(sim)
      shuffledDecks <- (1 to n).toList.traverse(_ => deck.shuffle[F])
      simResults = shuffledDecks.map(runThis(sim))
    } yield aggregateEquityFromMany(simResults)
}

// TODO Organization - what should be in Equity Service vs SimBoardState
sealed trait SimBoardState {
  def allHands(sim: SimSetup): List[(Position, Hand)] = {
    val boardCards: List[Card] = sim match {
      case SimSetup(_, card1, card2, card3, turn, river) =>
        List(card1, card2, card3, turn, river).collect { case Some(x) => x }
    }

    // TODO If this is safe I must fix the Hand Ranking function .
    sim.players.map { case SimPlayer(position, card1, card2) =>
      (
        position,
        Hand.rank(List(card1, card2).collect { case Some(x) =>
          x
        } ++ boardCards)
      )
    }
  }
}

object SimBoardState
case object PreFlop   extends SimBoardState
case object FlopCard1 extends SimBoardState
case object FlopCard2 extends SimBoardState
case object FlopCard3 extends SimBoardState
case object Turn      extends SimBoardState
case object River     extends SimBoardState
