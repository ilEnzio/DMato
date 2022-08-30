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
  val deck: IO[Deck]
}

object BoardState {
  def deal(players: List[Player]): IO[BoardState] = {
    val board: List[Card] = Nil

    // TODO if I return an IO here Does the whole BoardState become IO?
    val deck: IO[Deck] = Deck.makeStartingDeck.shuffle
    IO(Preflop(players, board, deck))
  }
  def deal(street: BoardState): IO[BoardState] = street match {
    case x: IO[Preflop] => ???
    case x: IO[Flop]    => ???
    case x: IO[Turn]    => ???
    case x: IO[River]   => x
  }

}
case class Preflop(players: List[Player], board: List[Card], deck: IO[Deck]) extends BoardState
case class Flop(players: List[Player], board: List[Card], deck: IO[Deck])    extends BoardState
case class Turn(players: List[Player], board: List[Card], deck: IO[Deck])    extends BoardState
case class River(players: List[Player], board: List[Card], deck: IO[Deck])   extends BoardState

case class Player(holeCards: (Card, Card)) {} // TODO a tuple is more accurate but harder to deal with

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
