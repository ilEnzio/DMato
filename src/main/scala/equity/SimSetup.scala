package equity

import cats._
import cats.syntax.all._
import cats.data.State
import cats.effect.{Async, IO}
import cats.effect.std.Random
import cats.implicits.catsSyntaxApplicativeId
import org.scalactic.anyvals.NonEmptySet
import poker.Street.River
import poker.{Card, Position, Rank, ShowDown, Suit}

final case class SimSetup[F[_]] private (
  // Do I even need a deck? The deck basically get derived
  // from the the cards that are prepopulated by the sim
  deck: SimDeck,
  players: List[SimPlayer[F]],
  card1: F[Card],
  card2: F[Card],
  card3: F[Card],
  turn: F[Card],
  river: F[Card]
) {

  def mapK[G[_]](f: F[Card] => G[Card]): SimSetup[G] = ???

}
object SimSetup {
  def applyK[F[_]: Random, Applicative](
    players: List[SimPlayer[F]],
    card1: F[Card],
    card2: F[Card],
    card3: F[Card],
    turn: F[Card],
    river: F[Card]
  ): SimSetup[Id] = ???

}

final case class SimPlayer[F[_]]( // TODO I think this is ??Option??
  position: Position,
  card1: F[Card],
  card2: F[Card]
) {
  // TODO I think my F is Option and my G is IO as I have to randomly
  // generate it.
  def mapK[G[_]](f: F[Card] => G[Card]): SimPlayer[G] = ???
}
object SimPlayer {
  def applyK[F[_]](
    position: Position,
    card1: F[Card],
    card2: F[Card]
  ): SimPlayer[F] = ???
}

final case class SimDeck(cards: List[Card])

final case class SimResult()

sealed trait EquityService[F[_], G[_]] {

  // TODO These are the two things you need to turn a set up
  // into a River board state.

  // Refactor out IO and Option
  def deckFrom: SimSetup[Option] => Id[SimDeck]
  def hydratedSim(sim: SimSetup[Option]): SimSetup[Option]

  def riverFrom: SimSetup[Option] => River
  def equity: List[River] => SimResult

  def runSim(simSetupOP: SimSetup[Option]): Option[NonEmptySet[Position]] = {
    val deck    = deckFrom(simSetupOP)
    val setUpID = hydratedSim(simSetupOP)
    val river   = riverFrom(setUpID)
    ShowDown.fromRiver(river)
  }

  def allSimResults(
    sim: SimSetup[Option],
    n: Int
  ): List[Option[NonEmptySet[Position]]] =
    (1 to n).toList.map(_ => runSim(sim))
}

object EquityService extends EquityService[Option, Id] {

  def getOrDeal(maybe: Option[Card], deck: SimDeck): (Card, SimDeck) =
    maybe.fold {
      // TODO not safe
      (deck.cards.head, SimDeck(deck.cards.tail))
    }(x => (x, deck))

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

  override def deckFrom: SimSetup[Option] => Id[SimDeck] = {

    // I need to generated a shuffled deck
    // Then I need to remove all cards given in the apply
    // then build the SimSetup[Option]

    def startingDeck: StartingDeckImpl = startingDeckImpl
    def collectAllFrom(simSetup: SimSetup[Option]): List[Card] = {
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

      val allPlayerCards = simSetup.players.foldLeft(List.empty[Card]) {
        (cards, player) =>
          cards ++ cardsFrom(player)
      }
      boardCards ++ allPlayerCards
    }

    sim: SimSetup[Option] =>
      val tempDeck = startingDeckImpl.cards
      val finalDeck = tempDeck
        .filterNot(collectAllFrom(sim).contains(_))
        .pure[Id]
      SimDeck(finalDeck)
  }

  override def hydratedSim(
    targetSim: SimSetup[Option]
  ): SimSetup[Option] = {

    // I think this is where I can use the State Monad
    // val simFlop: State[SimSetup[Option], SimDeck] ??? I don't understand what the
    // "result" is here....
    // I need to refactor this I think.  I think this part of the work of state monad....
    val simBoard = State[SimSetup[Option], Unit] { sim =>
      val (newCard1, newDeck)  = getOrDeal(sim.card1, sim.deck)
      val part1                = sim.copy(card1 = newCard1.pure[Option], deck = newDeck)
      val (newCard2, newDeck2) = getOrDeal(part1.card2, part1.deck)
      val part2                = part1.copy(card2 = newCard2.pure[Option], deck = newDeck2)
      val (newCard3, newDeck3) = getOrDeal(part2.card3, part2.deck)
      val part3                = part2.copy(card3 = newCard3.pure[Option], deck = newDeck3)
      val (newTurn, newDeck4)  = getOrDeal(part3.turn, part3.deck)
      val part4                = part3.copy(turn = newTurn.pure[Option], deck = newDeck4)
      val (newRiver, newDeck5) = getOrDeal(part4.card3, part4.deck)
      val part5                = part2.copy(river = newRiver.pure[Option], deck = newDeck5)
      (part5, ())
    }

    def hydratePlayer(
      player: SimPlayer[Option],
      simDeck: SimDeck
    ): (SimPlayer[Option], SimDeck) = {
      val (newPlCard1, newDeck6) = getOrDeal(player.card1, simDeck)
      val (newPlCard2, newDeck7) = getOrDeal(player.card2, newDeck6)
      val newPlayer = player.copy(
        card1 = newPlCard1.pure[Option],
        card2 = newPlCard2.pure[Option]
      )
      (newPlayer, newDeck7)
    }

    //  TODO something is wrong here... The signature doesn't quite help understand what's going on.
    val simPlayers: State[SimSetup[Option], Unit] =
      State[SimSetup[Option], Unit] { simSetup =>
        val (allNewPlayers, _) = simSetup.players.foldLeft(
          (List.empty[SimPlayer[Option]], simSetup.deck)
        ) { (sim, player) =>
          val (newPlayer, newDeck) = hydratePlayer(player, sim._2)
          (sim._1 :+ newPlayer, newDeck)

        }
        (simSetup.copy(players = allNewPlayers), ())
      }

    // TODO Refactor this
    val test = for {
      _ <- simBoard
      _ <- simPlayers
    } yield ()
    test.run(targetSim).value._1

  }

  override def riverFrom: SimSetup[Option] => River = ???

  override def equity: List[River] => SimResult = ???

}
