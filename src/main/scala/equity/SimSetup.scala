package equity

import cats._
import cats.syntax.all._
import cats.data.State
import cats.effect.{Async, IO}
import cats.effect.std.Random
import cats.implicits.catsSyntaxApplicativeId
import org.scalactic.anyvals.NonEmptySet
import poker.Street.River
import poker.{Card, Player, Position, Rank, ShowDown, Suit}

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

final case class SimPlayer[F[_]] private ( // TODO I think this is ??Option??
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
    val (newPlCard1, newDeck6) = EquityService.getOrDeal(card1, deck)
    val (newPlCard2, newDeck7) = EquityService.getOrDeal(card2, newDeck6)
    val newPlayer = SimPlayer(
      position,
      card1 = newPlCard1.some,
      card2 = newPlCard2.some
    )
    (newPlayer, newDeck7)
  }
}

final case class SimDeck(cards: List[Card])

final case class SimResult(winnersList: List[Position]) {}

final case class EquityCalculation(equities: Map[Position, List[Double]])

sealed trait EquityService[F[_], G[_]] {

  // Refactor out Id and Option??
  def deckFrom: SimSetup[F] => SimDeck

  def hydratedSim(sim: SimSetup[F])(simDeck: SimDeck): SimSetup[F]

  def riverFrom: SimSetup[F] => River

  def runThis(simSetupOp: SimSetup[F]): F[List[Position]]

  def equityFromSimResult(result: SimResult): EquityCalculation

  def finalEquityFromManySims(
    results: List[SimResult]
  ): EquityCalculation =
    results
      .map(equityFromSimResult)
      .foldLeft(
        EquityCalculation(Map.empty[Position, List[Double]])
      ) { (s, v) =>
        combinedEquity(s, v)
      }

  def combinedEquity(
    eq1: EquityCalculation,
    eq2: EquityCalculation
  ): EquityCalculation

  def equity(
    sim: SimSetup[F],
    n: Int
  ): Option[EquityCalculation]
}

object EquityService extends EquityService[Option, Id] {

  def getOrDeal(maybe: Option[Card], deck: SimDeck): (Card, SimDeck) =
    maybe.fold {
      // TODO not safe
      (deck.cards.head, SimDeck(deck.cards.tail))
    }(x => (x, deck))

  // TODO this isnt being used.
  final private case class StartingDeckImpl(cards: List[Card]) {
    def shuffle[F[_]: Functor: Random]: F[List[Card]] =
      Random[F].shuffleList(cards)

  }
  private def startingDeckImpl: StartingDeckImpl = {
    val cardList = for {
      rank <- Rank.all
      suit <- Suit.all
    } yield Card(rank, suit)
    StartingDeckImpl(cardList)
  }

  override def deckFrom: SimSetup[Option] => SimDeck = {

    def startingDeck: StartingDeckImpl = startingDeckImpl
    def allCardsFrom(simSetup: SimSetup[Option]): List[Card] = {
      val boardList1 = simSetup.card1.fold(List.empty[Card])(List(_))
      val boardList2 = simSetup.card2.fold(List.empty[Card])(List(_))
      val boardList3 = simSetup.card3.fold(List.empty[Card])(List(_))
      val boardList4 = simSetup.turn.fold(List.empty[Card])(List(_))
      val boardList5 = simSetup.river.fold(List.empty[Card])(List(_))
      val boardCards =
        boardList1 ++ boardList2 ++ boardList3 ++ boardList4 ++ boardList5

      // Player cards
      def cardsFrom = { player: SimPlayer[Option] =>
        val card1 = player.card1.fold(List.empty[Card])(List(_))
        val card2 = player.card2.fold(List.empty[Card])(List(_))
        card1 ++ card2
      }

      def allPlayerCards = simSetup.players.foldLeft(List.empty[Card]) {
        (cards, player) =>
          cards ++ cardsFrom(player)
      }
      boardCards ++ allPlayerCards
    }

    sim: SimSetup[Option] =>
      val finalDeck = startingDeck.cards
        .filterNot(allCardsFrom(sim).contains(_))
        .pure[Id]
      SimDeck(finalDeck)
  }

  override def hydratedSim(
    targetSim: SimSetup[Option]
  )(simDeck: SimDeck): SimSetup[Option] = {

    // val simFlop: State[SimSetup[Option], SimDeck] ??? I don't understand what the
    // "result" is here....
    type SimState[A] = State[(SimSetup[Option], SimDeck), A]

    sealed trait BoardState {}
    object BoardState {
      def after(state: BoardState): SimState[List[Card]] =
        State[(SimSetup[Option], SimDeck), List[Card]] { case (sim, deck) =>
          val (newCard, newDeck): (Card, SimDeck) = state match {
            case FlopCard1 => getOrDeal(sim.card1, deck)
            case FlopCard2 => getOrDeal(sim.card2, deck)
            case FlopCard3 => getOrDeal(sim.card3, deck)
            case Turn      => getOrDeal(sim.turn, deck)
            case River     => getOrDeal(sim.river, deck)
          }

          val newSim = state match {
            case FlopCard1 => sim.copy(card1 = newCard.pure[Option])
            case FlopCard2 => sim.copy(card2 = newCard.pure[Option])
            case FlopCard3 => sim.copy(card3 = newCard.pure[Option])
            case Turn      => sim.copy(turn = newCard.pure[Option])
            case River     => sim.copy(river = newCard.pure[Option])
          }
          ((newSim, newDeck), newDeck.cards)
        }
    }
    final case object FlopCard1 extends BoardState
    final case object FlopCard2 extends BoardState
    final case object FlopCard3 extends BoardState
    final case object Turn      extends BoardState
    final case object River     extends BoardState

    def hydratePlayer(player: SimPlayer[Option])(
      deck: SimDeck
    ): (SimPlayer[Option], SimDeck) =
      SimPlayer.applyK(player.position, player.card1, player.card2)(deck)

    val stateAfterPlayers: SimState[List[Card]] =
      State[(SimSetup[Option], SimDeck), List[Card]] { case (sim, deck) =>
        val (allNewPlayers, finalDeck) = sim.players.foldLeft(
          (List.empty[SimPlayer[Option]], deck)
        ) { case ((allPlayers, deck), player) =>
          val (newPlayer, newDeck) = hydratePlayer(player)(deck)
          (allPlayers :+ newPlayer, newDeck)
        }
        (
          (sim.copy(players = allNewPlayers), finalDeck),
          finalDeck.cards
        )
      }

    val simulation = for {
      _ <- BoardState.after(FlopCard1)
      _ <- BoardState.after(FlopCard2)
      _ <- BoardState.after(FlopCard3)
      _ <- BoardState.after(Turn)
      _ <- BoardState.after(River)
      _ <- stateAfterPlayers
    } yield ()

    simulation.runS(targetSim, simDeck).value._1

  }

  override def riverFrom: SimSetup[Option] => River = {
    def playerFrom(simPlayer: SimPlayer[Option]): Player =
      // TODO: Not safe
      Player(simPlayer.position, simPlayer.card1.get, simPlayer.card2.get)
    sim: SimSetup[Option] =>
      River(
        sim.players.map(playerFrom),
        sim.card1.get,
        sim.card2.get,
        sim.card3.get,
        sim.turn.get,
        sim.river.get
      )
  }

  override def runThis(simSetupOp: SimSetup[Option]): Option[List[Position]] = {
    val deck   = deckFrom(simSetupOp)
    val optSim = hydratedSim(simSetupOp)(deck)
    val river  = riverFrom(optSim)
    ShowDown.fromRiver(river).map(_.toList)
  }

  override def equityFromSimResult(result: SimResult): EquityCalculation = {
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

  override def equity(
    sim: SimSetup[Option],
    n: Int
  ): Option[EquityCalculation] =
    for {
      simResults <- (1 to n).toList
        .traverse(_ => runThis(sim).map(SimResult))
      equityOfHands = finalEquityFromManySims(simResults)
    } yield equityOfHands

}
