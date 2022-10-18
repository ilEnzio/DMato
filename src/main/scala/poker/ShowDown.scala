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

    val flop  = Street.deal(preflop)
    val turn  = Street.deal(flop)
    val river = Street.deal(turn)
    from(river)
  }

  def fromFlop(flop: Flop): Option[NonEmptySet[Int]] = {
    val turn  = Street.deal(flop)
    val river = Street.deal(turn)
    from(river)
  }

  def fromTurn(turn: Turn): Option[NonEmptySet[Int]] = {
    val river = Street.deal(turn)
    from(river)
  }

  def fromRiver(river: River): Option[NonEmptySet[Int]] = {
// TODO this map to reverse the zip seems goofy

    val hands: Seq[(Int, Hand)] = allHands(river)

    val handsSet: Set[Int] = hands
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

object PlayerStanding {
  def apply(board: Street): List[(Int, Player, Hand)] =
    board.players
      .zip(board.allHands)
      .zipWithIndex
      .map { case ((p, h), i) => (i, p, h) }

//TODO this must be wrong??
  def winnerList(board: Street): Option[NonEmptySet[(Int, Player, Hand)]] = {
    val winners = PlayerStanding(board).maximumByList { case (_, _, hand) => hand }.toSet

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
