package poker

import cats.data._
import cats.effect.IO
import Deck._
import cats.effect.unsafe.implicits.global

import scala.util.Random

// the idea here this that each "state" is a final case class
// so the state is going through pipeline
// then I can somehow use unapply methods
/// TODO: I don't understand the proper use of unapply.
/// I feel like I've just added a bunch of boiler plate

sealed trait Street {
  // TODO should allHands include the position of the hand?
  val allHands: List[Hand]
  val players: List[Player]
} // Street

object Street {

  final case class Preflop(players: List[Player])
      extends Street {

    val allHands: List[Hand] =
      players.map { case Player(_, x, y) => Hand.rank(List(x, y)) }
  }

//

  final case class Flop(
    players: List[Player],
    card1: Card,
    card2: Card,
    card3: Card
  ) extends Street {
    val allHands: List[Hand] =
      players.map { case Player(_, x, y) =>
        Hand.rank(List(x, y, card1, card2, card3))
      }
  }

  final case class Turn(
    players: List[Player],
    card1: Card,
    card2: Card,
    card3: Card,
    turn: Card
  ) extends Street {
    val allHands: List[Hand] =
      players.map { case Player(_, x, y) =>
        Hand.rank(List(x, y, card1, card2, card3, turn))
      }
  }

  final case class River(
    players: List[Player],
    card1: Card,
    card2: Card,
    card3: Card,
    turn: Card,
    river: Card
  ) extends Street {
    val allHands: List[Hand] =
      players.map { case Player(_, x, y) =>
        Hand.rank(List(x, y, card1, card2, card3, turn, river))
      }
  }

  /// State machine needs to go to the Deck. (FlopCards, FlopDeck)
  // Flop - street
  // FlopCards - the three cards
  def dealFlop(preflop: Preflop): State[Deck, Flop] = { // (FlopCards, FlopDeck)
    for {
      deck <- State.get[Deck]
      (flop, flopDeck) = deck.dealFlop
      _ <- State.set(flopDeck)
    } yield Flop(preflop.players, flop.card1, flop.card2, flop.card3)
  }

  def dealTurn(flop: Flop): State[Deck, Turn] =
    for {
      deck <- State.get[Deck]
      (turn, turnDeck) = deck.dealTurn
    } yield Turn(flop.players, flop.card1, flop.card2, flop.card3, turn.card)

  def dealRiver(turn: Turn): State[Deck, River] = {
    for {
      deck <- State.get[Deck]
      river = deck.dealRiver
      // TODO: should set a state, but dealRiver doesn't return it
    } yield River(
      turn.players,
      turn.card1,
      turn.card2,
      turn.card3,
      turn.turn,
      river.card
    )
  }

  /*
  val next: State[Street, List[Hand]] = State { (s: Street) =>
    s match {
      case x: Preflop =>
        val flop = dealFlop(x)
        (flop, flop.allHands)
      case x: Flop =>
        val turn: Turn = dealTurn(x)
        (turn, turn.allHands)
      case x: Turn =>
        val river = dealRiver(x)
        (river, river.allHands)
      case x: River => (x, x.allHands)
    }
   }
   */
// TODO I really like to generalize this.  Maybe it's the State instance that needs to be created?
  // maybe State Monad is an alternative model for what I'm doing already?
  def runOutPreFlopToRiver(preflop: Preflop): State[Deck, River] = for {
    flop <- Street.dealFlop(preflop)
    river <- runOutFlopToRiver(flop)
    // this seems cool for future.  but now it's more complex to get the value out.
  } yield river

  def runOutFlopToRiver(flop: Flop): State[Deck, River] = for {
    turn  <- Street.dealTurn(flop)
    river <- runOutTurnToRiver(turn)
  } yield river

  def runOutTurnToRiver(turn: Turn): State[Deck, River] =
    Street.dealRiver(turn)
}

sealed trait Position {}
object Position       {
  // TODO this position is not the best model
  val positionMap: Map[Int, Position] = Map(
    1  -> SmallBlind,
    2  -> BigBlind,
    3  -> UTG,
    4  -> UTGP1,
    5  -> UTGP2,
    6  -> UTGP3,
    7  -> LoJack,
    8  -> HighJack,
    9  -> CutOff,
    10 -> Button
  )
}
case object SmallBlind extends Position
case object BigBlind   extends Position
case object UTG        extends Position
case object UTGP1      extends Position
case object UTGP2      extends Position
case object UTGP3      extends Position
case object LoJack     extends Position
case object HighJack   extends Position
case object CutOff     extends Position
case object Button     extends Position

case class Player(position: Position, card1: Card, card2: Card) // position ??
