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
  // Do I even need a deck? The deck basically get derived
  // from the the cards that are prepopulated by the sim
//  deck: F[SimDeck],
  players: List[
    SimPlayer[F]
  ], // TODO Should I type this to make subsequent code more expressive?
  card1: F[Card],
  card2: F[Card],
  card3: F[Card],
  turn: F[Card],
  river: F[Card]
) {

  // I think this is hydrate...
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
//    //
//    val sim =
//      SimSetup(players, card1, card2, card3, turn, river)
//    val deck = EquityService.deckFrom(sim)
//    EquityService
//      .hydratedSim(sim)(deck)
//
//  }
}

final case class SimPlayer[F[_]] private ( // TODO I think this is ??Option??
  position: Position,
  card1: F[Card],
  card2: F[Card]
) {

  def mapK[G[_]](f: F[Card] => G[Card]): SimPlayer[G] =
    copy(card1 = f(card1), card2 = f(card2))

}
object SimPlayer {
  // TODO refactor??  this seems to be the same as
  // what's happening in hydrateSim/hydratePlayer
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

final case class SimResult()

sealed trait EquityService[F[_], G[_]] {

  // TODO These are the two things you need to turn a set up
  // into a River board state.

  // Refactor out Id and Option??
  def deckFrom: SimSetup[Option] => Id[SimDeck]
  def hydratedSim(sim: SimSetup[Option])(simDeck: SimDeck): SimSetup[Option]

  def riverFrom: SimSetup[Option] => River
  def equity: List[River] => SimResult

  def runSim(simSetupOp: SimSetup[Option]): Option[NonEmptySet[Position]] = {
    val deck     = deckFrom(simSetupOp)
    val setUpOpt = hydratedSim(simSetupOp)(deck)
    val river    = riverFrom(setUpOpt)
    ShowDown.fromRiver(river)
  }

  def allSimResults(
    sim: SimSetup[Option],
    n: Int
  ): List[Option[NonEmptySet[Position]]] =
    (1 to n).toList.map(_ => runSim(sim))
}

object EquityService extends EquityService[Option, Id] {

  // TODO Maybe this should also be using the State Monad
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

  override def deckFrom: SimSetup[Option] => SimDeck = {

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
  )(simDeck: SimDeck): SimSetup[Option] = {

    // I think this is where I can use the State Monad
    // val simFlop: State[SimSetup[Option], SimDeck] ??? I don't understand what the
    // "result" is here....
    // I need to refactor this I think.  I think this part of the work of state monad....
    val simBoard =
      State[SimSetup[Option], SimDeck] { sim =>
        val (newCard1, deck)  = getOrDeal(sim.card1, simDeck)
        val sim1              = sim.copy(card1 = newCard1.pure[Option])
        val (newCard2, deck2) = getOrDeal(sim1.card2, deck)
        val sim2              = sim1.copy(card2 = newCard2.pure[Option])
        val (newCard3, deck3) = getOrDeal(sim2.card3, deck2)
        val sim3              = sim2.copy(card3 = newCard3.pure[Option])
        val (newTurn, deck4)  = getOrDeal(sim3.turn, deck3)
        val sim4              = sim3.copy(turn = newTurn.pure[Option])
        val (newRiver, deck5) = getOrDeal(sim4.card3, deck4)
        val sim5              = sim2.copy(river = newRiver.pure[Option])
        (sim5, deck5)
      }

    def hydratePlayer(player: SimPlayer[Option])(
      deck: SimDeck
    ): (SimPlayer[Option], SimDeck) =
      SimPlayer.applyK(player.position, player.card1, player.card2)(deck)

    //  TODO something is wrong here... The signature doesn't quite help understand what's going on.
    val simPlayers: State[SimSetup[Option], SimDeck] =
      State[SimSetup[Option], SimDeck] { simSetup =>
        val (allNewPlayers, finalDeck) = simSetup.players.foldLeft(
          (List.empty[SimPlayer[Option]], simDeck)
        ) { case ((allPlayers, deck), player) =>
          val (newPlayer, newDeck) = hydratePlayer(player)(deck)
          (allPlayers :+ newPlayer, newDeck)
        }
        (simSetup.copy(players = allNewPlayers), finalDeck)
      }

    // TODO That is this???
    val test = for {
      _ <- simBoard
      _ <- simPlayers
    } yield ()
    val test2 = test.run(targetSim).value._1
    test2
//    val test3 = test2.copy(
//      card1 = test2.card1.pure[Id],
//      card2 = test2.card2.pure[Id],
//      card3 = test2.card3.pure[Id],
//      turn = test2.turn.pure[Id],
//      river = test2.river.pure[Id],
//      players = test2.players.map { p =>
//        p.mapK { c: Option[Card] => c.get.pure[Id] }
//      }
//    )
//    test3

  }

  override def riverFrom: SimSetup[Option] => River = {
    def playerFrom(simPlayer: SimPlayer[Option]): Player =
      // TODO: Not safe
      Player(simPlayer.position, simPlayer.card1.get, simPlayer.card2.get)
    sim: SimSetup[Option] =>
      River(
        sim.players.map(playerFrom(_)),
        sim.card1.get,
        sim.card2.get,
        sim.card3.get,
        sim.turn.get,
        sim.river.get
      )
  }

  override def equity: List[River] => SimResult = ???

}
