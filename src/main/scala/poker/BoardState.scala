package poker

import cats.effect.IO
import cats.implicits._
import cats.kernel.Monoid
import poker.Hand_2.UnrankedHand

import scala.util.Random

// the idea here this that each "state" is a final case class
// so the state is going through pipeline
// then I can somehow use unapply methods
/// TODO: I don't understand the proper use of unapply.
/// I feel like I've just added a bunch of boiler plate

sealed trait BoardState

// TODO: I'm not sure I understand why it was suggested I put this in an object.
// and I think I've gotten this wrong...

object BoardState {

  def deal(players: List[Player]): IO[BoardState] = {
    val deck: IO[Deck] = Deck.makeStartingDeck.shuffle
    deck.map(Preflop(players, _))
  }

//TODo: come back and fix the unapply
  def deal(boardState: BoardState): BoardState =
    boardState match {
//      case Preflop(ps, d)                        => dealFlop(ps, d)
//      case BoardState.Flop(ps, d, c1, c2, c3)    => dealTurn(ps, d, c1, c2, c3)
//      case BoardState.Turn(ps, d, c1, c2, c3, t) => dealRiver(ps, d, c1, c2, c3, t)
//      case BoardState.River(ps, d, c1, c2, c3, t, r) => ???
      case x: Preflop => dealFlop(x.players, x.deck) // flop factory
      case x: Flop    => dealTurn(x.players, x.deck, x.card1, x.card2, x.card3)
      case x: Turn    => dealRiver(x.players, x.deck, x.card1, x.card2, x.card3, x.turn)
      case x: River   => x
    }

  final case class Preflop(players: List[Player], deck: Deck) extends BoardState {
    def allHands: List[Hand_2] =
      players.map { case Player(x, y) => Hand_2.rank(List(x, y)) }
  }
  final case class Flop(players: List[Player], deck: Deck, card1: Card, card2: Card, card3: Card) extends BoardState {
    def allHands: List[Hand_2] =
      players.map { case Player(x, y) => Hand_2.rank(List(x, y, card1, card2, card3)) }
  }
  final case class Turn(players: List[Player], deck: Deck, card1: Card, card2: Card, card3: Card, turn: Card)
      extends BoardState
  final case class River(
    players: List[Player],
    deck: Deck,
    card1: Card,
    card2: Card,
    card3: Card,
    turn: Card,
    river: Card
  ) extends BoardState
// TODo Refactor all the unapplys
  object Preflop {
    def unapply(state: Preflop): Option[(List[Player], Deck)] =
      // TODO: What's suppose to happen here?  validation?
      Some(state.players, state.deck)

  }
  object Flop {
    def unapply(flop: Flop): Option[(List[Player], Deck, Card, Card, Card)] =
      Some(flop.players, flop.deck, flop.card1, flop.card2, flop.card3)
  }

  object Turn {
    def unapply(turn: Turn): Option[(List[Player], Deck, Card, Card, Card, Card)] =
      Some(turn.players, turn.deck, turn.card1, turn.card2, turn.card3, turn.turn)
  }

  object River {
    def unapply(river: River): Option[(List[Player], Deck, Card, Card, Card, Card, Card)] =
      Some(river.players, river.deck, river.card1, river.card2, river.card3, river.turn, river.river)
  }
// TODO none of this is safe??
  def dealFlop(players: List[Player], deck: Deck): BoardState.Flop = {
    val newDeck = deck.drop(3)
    val flop    = deck.take(3)
    Flop(players, newDeck, flop(0), flop(1), flop(2))
  }

  def dealTurn(players: List[Player], deck: Deck, fl1: Card, fl2: Card, fl3: Card): Turn = {
    val newDeck = deck.drop(1)
    val turn    = deck.take(1)
    Turn(players, newDeck, fl1, fl2, fl3, turn.headOption.get)
  }

  def dealRiver(players: List[Player], deck: Deck, fl1: Card, fl2: Card, fl3: Card, t: Card): River = {
    val newDeck = deck.drop(1)
    val river   = deck.take(1)
    River(players, newDeck, fl1, fl2, fl3, t, river.headOption.get)
  }

}

//
//sealed trait BoardState {
//  // players/players hands from 2 - 9
//  // board cards 0 - 5
//  // deck(remaining cards)
//  val players: List[Player]
//  val board: List[Card] // 1, 2, 3, 4, 5
//  val deck: Deck
//}
//
//// ToDo where do those initial players come from??
//// I need the player creation / hole card selection.
//// or I need a mock for now.
//object BoardState {
//  def deal(players: List[Player]): IO[BoardState] = {
//    val deck: IO[Deck] = Deck.makeStartingDeck.shuffle
//    deck.map(Preflop(players, _))
//  }
//
//  def deal(street: BoardState): BoardState = street match {
//    case x: Preflop => ???
//    case x: Flop    => ???
//    case x: Turn    => ???
//    case x: River   => x
//  }
//}
//case class Preflop(players: List[Player], deck: Deck) extends BoardState {
//  override val board: List[Card] = Nil
//}
//case class Flop(players: List[Player], card1: Card, card2: Card, card3: Card, deck: Deck) extends BoardState {
//  override val board: List[Card] = List(card1, card2, card3)
//}
//case class Turn(players: List[Player], card1: Card, card2: Card, card3: Card, turn: Card, deck: Deck)
//    extends BoardState {
//  override val board: List[Card] = List(card1, card2, card3, turn)
//}
//case class River(players: List[Player], card1: Card, card2: Card, card3: Card, turn: Card, river: Card, deck: Deck)
//    extends BoardState {
//  override val board: List[Card] = List(card1, card2, card3, turn, river)
//}

case class Player(card1: Card, card2: Card)

case class WinnerList(map: Map[Int, Int]) // ToDo Player position, rather than int
object WinnerList {

  implicit val winnerListMonoid: Monoid[WinnerList] = new Monoid[WinnerList] {
    override def empty: WinnerList = WinnerList(Map.empty)

    override def combine(x: WinnerList, y: WinnerList): WinnerList =
      WinnerList(y.map.foldLeft(x.map) { (s, v) =>
        if (!s.contains(v._1)) s + v
        else s + (v._1 -> (v._2 + s(v._1)))
      })
  }
}
