package poker

import cats.effect.IO
import cats.kernel.Monoid

import scala.util.Random

sealed trait BoardState {
  // players/players hands from 2 - 9
  // board cards 0 - 5
  // deck(remaining cards)
  val players: List[Player]
  val board: List[Card]
  val deck: Deck
}

object BoardState {
  def deal(players: List[Player]): BoardState = {
    val board: List[Card] = Nil
    val deck: Deck        = ???
    Preflop(players, board, deck)
  }
  def deal(street: BoardState): BoardState = street match {
    case x: Preflop => Flop(x.players, ???, ???)
    case x: Flop    => Turn(x.players, ???, ???)
    case x: Turn    => River(x.players, ???, ???)
    case x: River   => x
  }
//  def shuffleDeck(deck: Deck): IO[Deck] =
//    IO(Deck(Random.shuffle(deck.cards)))

}
case class Preflop(players: List[Player], board: List[Card], deck: Deck) extends BoardState {}
case class Flop(players: List[Player], board: List[Card], deck: Deck)    extends BoardState
case class Turn(players: List[Player], board: List[Card], deck: Deck)    extends BoardState
case class River(players: List[Player], board: List[Card], deck: Deck)   extends BoardState

case class Player(holeCards: (Card, Card)) {}

case class WinnerList(map: Map[Int, Int]) {}
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
