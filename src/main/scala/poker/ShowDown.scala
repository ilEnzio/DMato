package poker

import cats.implicits._
import poker.BoardState.River
import poker.OrderInstances._
import cats.kernel.Monoid

sealed trait ShowDown

object ShowDown {
  def apply(hands: List[Hand]): List[Hand] =
    hands.maximumList

  def fromRiver(board: River): WinnerList = {
// TODO this map to reverse the zip seems goofy

    // TODO: Also need to refactor/extract the assembling of the hands, because I can use that for the tests
    val hands = board.players
      .map { case Player(x, y) =>
        Hand.rank(List(x, y, board.card1, board.card2, board.card3, board.turn, board.river))
      }
      .zipWithIndex
      .map { case (x, y) => (y, x) }

    hands
      .maximumByList[Hand](x => x._2)
      .map { case (player, _) => WinnerList(Map(player -> 1)) }
      .foldLeft(WinnerList.initial(board.players.size))(_ |+| _)
  }

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
