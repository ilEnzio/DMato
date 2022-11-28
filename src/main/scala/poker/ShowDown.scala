package poker

import cats.implicits._
import poker.Street.{Flop, Preflop, River, Turn}
import poker.OrderInstances._
import cats.kernel.Monoid
import org.scalactic.anyvals.NonEmptySet

sealed trait ShowDown

object ShowDown {
  // TODO - this is a strange api choice
  def apply(hands: List[Hand]): List[Hand] =
    hands.maximumList

  // what I might do is make the show down pass through all states
  // then parameterize fromRiver from[A :< River]

  def from[A >: Street](board: A): Option[NonEmptySet[Int]] =
    board match {
      case x: Preflop => fromPreFlop(x)
      case x: Flop    => fromFlop(x)
      case x: Turn    => fromTurn(x)
      case x: River   => fromRiver(x)
    }

  def fromPreFlop(preflop: Preflop): Option[NonEmptySet[Int]] = {

    val flop  = Street.dealFlop(preflop)
    val turn  = Street.dealTurn(flop)
    val river = Street.dealRiver(turn)
    from(river)
  }

  def fromFlop(flop: Flop): Option[NonEmptySet[Int]] = {
    val turn  = Street.dealTurn(flop)
    val river = Street.dealRiver(turn)
    from(river)
  }

  def fromTurn(turn: Turn): Option[NonEmptySet[Int]] = {
    val river = Street.dealRiver(turn)
    from(river)
  }

  def fromRiver(river: River): Option[NonEmptySet[Int]] = {
// TODO this map to reverse the zip seems goofy

    val handsSet: Set[Int] = river.allHands
      .maximumByList[Hand](x => x._2)
      .map { case (player, _) => player }
      .toSet

    NonEmptySet.from(handsSet)
  }

}

object PlayerStanding {
  def apply(board: Street): List[(Int, Player, Hand)] =
    board.players
      .zip(board.allHoleCardHands)
      .zipWithIndex
      .map { case ((p, h), i) => (i, p, h) }

//TODO this must be wrong??
  def winnerList(board: Street): Option[NonEmptySet[(Int, Player, Hand)]] = {
    val winners: Set[(Int, Player, Hand)] = PlayerStanding(board)
      .maximumByList[Hand](x => x._3)
      .toSet

    NonEmptySet.from(winners)
  }
}

case class WinnerList(map: Map[Int, Int])
object WinnerList {
  def initial(n: Int)(implicit m: Monoid[WinnerList]): WinnerList =
    (1 to n)
      .map(x => WinnerList(Map(x -> 0)))
      .foldLeft(m.empty)(m.combine)

  implicit val winnerListMonoid: Monoid[WinnerList] = new Monoid[WinnerList] {
    override def empty: WinnerList = WinnerList(Map.empty)

    override def combine(x: WinnerList, y: WinnerList): WinnerList =
      WinnerList(y.map.foldLeft(x.map) { (s, v) =>
        if (!s.contains(v._1)) s + v
        else s + (v._1 -> (v._2 + s(v._1)))
      })
  }
}
