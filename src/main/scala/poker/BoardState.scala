package poker

import cats.effect.IO
import cats.kernel.Monoid

import scala.util.Random

sealed trait BoardState {
  // players/players hands from 2 - 9
  // board cards 0 - 5
  // deck(remaining cards)
  val players: List[Player]
  val board: List[Card] // 1, 2, 3, 4, 5
  val deck: Deck
}

// ToDo where do those initial players come from??
// I need the player creation / hole card selection.
// or I need a mock for now.
object BoardState {
  def deal(players: List[Player]): IO[BoardState] = {
    val deck: IO[Deck] = Deck.makeStartingDeck.shuffle
    deck.map(Preflop(players, _))
  }

  def deal(street: BoardState): BoardState = street match {
    case x: Preflop => ???
    case x: Flop    => ???
    case x: Turn    => ???
    case x: River   => x
  }
}
case class Preflop(players: List[Player], deck: Deck) extends BoardState {
  override val board: List[Card] = Nil
}
case class Flop(players: List[Player], card1: Card, card2: Card, card3: Card, deck: Deck) extends BoardState {
  override val board: List[Card] = List(card1, card2, card3)
}
case class Turn(players: List[Player], card1: Card, card2: Card, card3: Card, turn: Card, deck: Deck)
    extends BoardState {
  override val board: List[Card] = List(card1, card2, card3, turn)
}
case class River(players: List[Player], card1: Card, card2: Card, card3: Card, turn: Card, river: Card, deck: Deck)
    extends BoardState {
  override val board: List[Card] = List(card1, card2, card3, turn, river)
}

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
