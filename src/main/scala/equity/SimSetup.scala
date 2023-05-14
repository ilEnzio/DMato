package equity

import cats._
import cats.syntax.all._
import cats.data.State
import cats.effect.{Async, IO}
import cats.effect.std.Random
import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxApplicativeId
import equity.EquityService.{equityFrom, getOrDeal, SimDeckState, SimState}
import org.scalactic.anyvals.NonEmptySet
import poker.OrderInstances.handOrder
import poker.{Card, Hand, Player, Position, Rank, ShowDown, Suit}

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

    val getOrDealPlayerCards = for {
      cardA <- getOrDeal(card1)
      cardB <- getOrDeal(card2)
    } yield SimPlayer(position, cardA.some, cardB.some)
    getOrDealPlayerCards.run(deck).value.swap
  }
}

final case class SimDeck(cards: List[Card])

final case class SimResult(winnersList: List[Position]) {}

final case class EquityCalculation(equities: Map[Position, List[Double]])

sealed trait EquityService[F[_]] {

  type SimState[A] = State[(SimSetup[Option], SimDeck), A]

  type SimDeckState[A] = State[SimDeck, A]

  def deckFrom: SimSetup[F] => SimDeck => SimDeck

  def hydratedSim: SimState[EquityCalculation]

  def runThis(simSetupOp: SimSetup[F])(simDeck: SimDeck): EquityCalculation

  def equityFrom(result: SimResult): EquityCalculation

  def combinedEquity(
    eq1: EquityCalculation,
    eq2: EquityCalculation
  ): EquityCalculation

  def theFinalEquityOf(
    sim: SimSetup[F],
    n: Int
  )(simDeck: SimDeck): EquityCalculation

  def finalEquityFromManyEquities(
    results: List[EquityCalculation]
  ): EquityCalculation =
    results
      .foldLeft(
        EquityCalculation(Map.empty[Position, List[Double]])
      ) { (s, v) =>
        combinedEquity(s, v)
      }
}

object EquityService extends EquityService[Option] {

  val dealOneCard: SimDeckState[Card] = State[SimDeck, Card] { case deck =>
    deck.cards match {
      case card :: remaining => (SimDeck(remaining), card)
      //      case Nil => ???
    }
  }

  val getOrDeal: Option[Card] => SimDeckState[Card] = optCard =>
    for {
      newCard <- optCard match {
        case Some(card) => State.pure[SimDeck, Card](card)
        case None       => EquityService.dealOneCard
      }
    } yield newCard

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

  override def deckFrom: SimSetup[Option] => SimDeck => SimDeck = {

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
      simDeck: SimDeck =>
        val finalDeck = simDeck.cards
          .filterNot(allCardsFrom(sim).contains(_))
          .pure[Id]
        SimDeck(finalDeck)
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
        SimBoardState.preFlopEquity(newSim)
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

  // TODO Rename this for heavens sake!!  This is the actual business
  override def theFinalEquityOf(
    sim: SimSetup[Option],
    n: Int
  )(deck: SimDeck): EquityCalculation = {
    val simResults = (1 to n).toList.map(_ => runThis(sim)(deck))
    finalEquityFromManyEquities(simResults)
  }
}

sealed trait SimBoardState {
  def allHands(sim: SimSetup[Option]): List[(Position, Hand)]
}

object SimBoardState {
  def preFlopEquity(sim: SimSetup[Option]): EquityCalculation = {
    val simResult: SimResult = winners(
      PreFlop
        .allHands(sim)
    )
    EquityService.equityFrom(simResult)
  }

  def winners(l: List[(Position, Hand)]): SimResult =
    SimResult(
      l.maximumByList[Hand](x => x._2)
        .map { case (playerPosition, _) => playerPosition }
    )

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

      val equityCalculation = state match {
        case PreFlop =>
          equityFrom(SimBoardState.winners(PreFlop.allHands(newSim)))
        case FlopCard1 =>
          equityFrom(SimBoardState.winners(FlopCard1.allHands(newSim)))
        case FlopCard2 =>
          equityFrom(SimBoardState.winners(FlopCard2.allHands(newSim)))
        case FlopCard3 =>
          equityFrom(SimBoardState.winners(FlopCard3.allHands(newSim)))
        case Turn => equityFrom(SimBoardState.winners(Turn.allHands(newSim)))
        case River =>
          equityFrom(SimBoardState.winners(River.allHands(newSim)))
      }
      ((newSim, newDeck), equityCalculation)
    }
}

final case object PreFlop extends SimBoardState {
  override def allHands(sim: SimSetup[Option]): List[(Position, Hand)] =
    sim.players
      .map { case SimPlayer(pos, Some(c1), Some(c2)) =>
        (
          pos,
          Hand.rank(
            List(
              c1,
              c2
            )
          )
        )
      }
}
final case object FlopCard1 extends SimBoardState {
  override def allHands(sim: SimSetup[Option]): List[(Position, Hand)] =
    sim.players
      .map { case SimPlayer(pos, Some(c1), Some(c2)) =>
        (
          pos,
          Hand.rank(
            List(
              c1,
              c2,
              sim.card1.get
            )
          )
        )
      }
}
final case object FlopCard2 extends SimBoardState {
  override def allHands(sim: SimSetup[Option]): List[(Position, Hand)] =
    sim.players
      .map { case SimPlayer(pos, Some(c1), Some(c2)) =>
        (
          pos,
          Hand.rank(
            List(
              c1,
              c2,
              sim.card1.get,
              sim.card2.get
            )
          )
        )
      }
}
final case object FlopCard3 extends SimBoardState {
  override def allHands(sim: SimSetup[Option]): List[(Position, Hand)] =
    sim.players
      .map { case SimPlayer(pos, Some(c1), Some(c2)) =>
        (
          pos,
          Hand.rank(
            List(
              c1,
              c2,
              sim.card1.get,
              sim.card2.get,
              sim.card3.get
            )
          )
        )
      }
}
final case object Turn extends SimBoardState {
  override def allHands(sim: SimSetup[Option]): List[(Position, Hand)] =
    sim.players
      .map { case SimPlayer(pos, Some(c1), Some(c2)) =>
        (
          pos,
          Hand.rank(
            List(
              c1,
              c2,
              sim.card1.get,
              sim.card2.get,
              sim.card3.get,
              sim.turn.get
            )
          )
        )
      }
}
final case object River extends SimBoardState {
  override def allHands(sim: SimSetup[Option]): List[(Position, Hand)] =
    sim.players
      .map { case SimPlayer(pos, Some(c1), Some(c2)) =>
        (
          pos,
          Hand.rank(
            List(
              c1,
              c2,
              sim.card1.get,
              sim.card2.get,
              sim.card3.get,
              sim.turn.get,
              sim.river.get
            )
          )
        )
      }
}
