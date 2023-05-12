package equity

import cats._
import cats.syntax.all._
import cats.data.State
import cats.effect.{Async, IO}
import cats.effect.std.Random
import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxApplicativeId
import org.scalactic.anyvals.NonEmptySet
import poker.OrderInstances.handOrder
import poker.Street.River
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

  type SimState[A] = State[(SimSetup[Option], SimDeck), A]

  // Refactor out Id and Option??
  def deckFrom: SimSetup[F] => SimDeck

  def hydratedSim: SimState[EquityCalculation]

  def runThis(simSetupOp: SimSetup[F]): EquityCalculation

  def equityFrom(result: SimResult): EquityCalculation

  def finalEquityFromManySims(
    results: List[EquityCalculation]
  ): EquityCalculation =
    results
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
  ): EquityCalculation
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

  override def hydratedSim: SimState[EquityCalculation] = {

    sealed trait BoardState {
      def allHands(sim: SimSetup[Option]): List[(Position, Hand)]
    }
    object BoardState {

      def preFlopEquity(sim: SimSetup[Option]): EquityCalculation = {
        val results: SimResult = winners(
          PreFlop
            .allHands(sim)
        )

        equityFrom(results)
      }

      def winners(l: List[(Position, Hand)]): SimResult =
        SimResult(
          l.maximumByList[Hand](x => x._2)
            .map { case (playerPosition, _) => playerPosition }
        )

      def after(state: BoardState): SimState[EquityCalculation] =
        State[(SimSetup[Option], SimDeck), EquityCalculation] {
          case (sim, deck) =>
            val (newCard, newDeck) = state match {
              // TODO this is wonky but kinda acts as a "burn" card...
              // Must change this.
              case PreFlop   => getOrDeal(Option.empty[Card], deck)
              case FlopCard1 => getOrDeal(sim.card1, deck)
              case FlopCard2 => getOrDeal(sim.card2, deck)
              case FlopCard3 => getOrDeal(sim.card3, deck)
              case Turn      => getOrDeal(sim.turn, deck)
              case River     => getOrDeal(sim.river, deck)
            }

            val newSim = state match {
              case PreFlop   => sim
              case FlopCard1 => sim.copy(card1 = newCard.pure[Option])
              case FlopCard2 => sim.copy(card2 = newCard.pure[Option])
              case FlopCard3 => sim.copy(card3 = newCard.pure[Option])
              case Turn      => sim.copy(turn = newCard.pure[Option])
              case River     => sim.copy(river = newCard.pure[Option])
            }

            // This is for equity calculation
            val eqCal = state match {
              case PreFlop =>
                equityFrom(BoardState.winners(PreFlop.allHands(newSim)))
              case FlopCard1 =>
                equityFrom(BoardState.winners(FlopCard1.allHands(newSim)))
              case FlopCard2 =>
                equityFrom(BoardState.winners(FlopCard2.allHands(newSim)))
              case FlopCard3 =>
                equityFrom(BoardState.winners(FlopCard3.allHands(newSim)))
              case Turn => equityFrom(BoardState.winners(Turn.allHands(newSim)))
              case River =>
                equityFrom(BoardState.winners(River.allHands(newSim)))
            }

            ((newSim, newDeck), eqCal)
        }
    }
    final case object PreFlop extends BoardState {
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
    final case object FlopCard1 extends BoardState {
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
    final case object FlopCard2 extends BoardState {
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
    final case object FlopCard3 extends BoardState {
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
    final case object Turn extends BoardState {
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
    final case object River extends BoardState {
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

    def hydratePlayer(player: SimPlayer[Option])(
      deck: SimDeck
    ): (SimPlayer[Option], SimDeck) =
      SimPlayer.applyK(player.position, player.card1, player.card2)(deck)

    val stateAfterPlayers: SimState[EquityCalculation] =
      State[(SimSetup[Option], SimDeck), EquityCalculation] {
        case (sim, deck) =>
          val (allNewPlayers, finalDeck) = sim.players.foldLeft(
            (List.empty[SimPlayer[Option]], deck)
          ) { case ((allPlayers, deck), player) =>
            val (newPlayer, newDeck) = hydratePlayer(player)(deck)
            (allPlayers :+ newPlayer, newDeck)
          }

          val newSim = sim.copy(players = allNewPlayers)

          (
            (newSim, finalDeck),
            BoardState.preFlopEquity(newSim)
          )
      }

    val simulationResult = for {
      _     <- stateAfterPlayers
      _     <- BoardState.after(FlopCard1)
      _     <- BoardState.after(FlopCard2)
      _     <- BoardState.after(FlopCard3)
      _     <- BoardState.after(Turn)
      river <- BoardState.after(River)
    } yield river

    simulationResult

  }

  override def runThis(simSetupOp: SimSetup[Option]): EquityCalculation = {
    val deck = deckFrom(simSetupOp)
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

  // TODO Rename this for heavens sake!!  This is the actually business
  override def equity(
    sim: SimSetup[Option],
    n: Int
  ): EquityCalculation = {

    val simResults = (1 to n).toList.map(_ => runThis(sim))
    finalEquityFromManySims(simResults)
  }

}
