package poker

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
  val allHands: List[Hand]
  val players: List[Player]
  def dealTillRiver: Street.River
} // Street

object Street {

  final case class Preflop(players: List[Player], deck: PreflopDeck)
      extends Street {

    val allHands: List[Hand] =
      players.map { case Player(_, x, y) => Hand.rank(List(x, y)) }

    /// State machine needs to go to the Deck. (FlopCards, FlopDeck)
    // Flop - street
    // FlopCards - the three cards
    def dealFlop: Flop = { // (FlopCards, FlopDeck)

      val (flop, flopDeck) = deck.dealFlop
      Flop(players, flopDeck, flop.card1, flop.card2, flop.card3)
    }

    override def dealTillRiver = dealFlop.dealTillRiver
  }

  final case class Flop(
    players: List[Player],
    deck: FlopDeck,
    card1: Card,
    card2: Card,
    card3: Card
  ) extends Street {
    val allHands: List[Hand] =
      players.map { case Player(_, x, y) =>
        Hand.rank(List(x, y, card1, card2, card3))
      }

    def dealTurn: Turn = {
      val (turn, turnDeck) = deck.dealTurn

      Turn(players, turnDeck, card1, card2, card3, turn.card)
    }

    override def dealTillRiver = dealTurn.dealTillRiver
  }

  final case class Turn(
    players: List[Player],
    deck: TurnDeck,
    card1: Card,
    card2: Card,
    card3: Card,
    turn: Card
  ) extends Street {
    val allHands: List[Hand] =
      players.map { case Player(_, x, y) =>
        Hand.rank(List(x, y, card1, card2, card3, turn))
      }

    def dealRiver: River = {
      val river = deck.dealRiver
      River(
        players,
        card1,
        card2,
        card3,
        turn,
        river.card
      )
    }

    override def dealTillRiver = dealRiver
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

    override val dealTillRiver = this
  }

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
