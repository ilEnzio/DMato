package poker

import cats.implicits._
import poker.BoardState.River
import poker.OrderInstances._
import cats.kernel.Monoid
import org.scalactic.anyvals.NonEmptySet

sealed trait ShowDown

object ShowDown {
  def apply(hands: List[Hand]): List[Hand] =
    hands.maximumList

  // what I might do is make the show down pass through all states
  // then parameterize fromRiver from[A :< River]

  def fromRiver(board: River): Option[NonEmptySet[Int]] = {
// TODO this map to reverse the zip seems goofy

    val hands = allHands(board)

    val handsSet = hands
      .maximumByList[Hand](x => x._2)
      .map { case (player, _) => player }
      .toSet

    NonEmptySet.from(handsSet)
  }

  def allHands(board: River): List[(Int, Hand)] =
    board.players
      .map { case Player(x, y) =>
        Hand.rank(List(x, y, board.card1, board.card2, board.card3, board.turn, board.river))
      }
      .zipWithIndex
      .map { case (x, y) => (y + 1, x) }
}

case class WinnerList(map: Map[Int, Int]) // ToDo Player position, rather than int ??
object WinnerList {
  def initial(n: Int): WinnerList =
    (1 to n)
      .map(x => WinnerList(Map(x -> 0)))
      .foldLeft(WinnerList.winnerListMonoid.empty)(_ |+| _)

  implicit val winnerListMonoid: Monoid[WinnerList] = new Monoid[WinnerList] {
    override def empty: WinnerList = WinnerList(Map.empty)

    override def combine(x: WinnerList, y: WinnerList): WinnerList =
      WinnerList(y.map.foldLeft(x.map) { (s, v) =>
        if (!s.contains(v._1)) s + v
        else s + (v._1 -> (v._2 + s(v._1)))
      })
  }
}
