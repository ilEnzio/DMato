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

  def from[A >: Street](board: A): Option[NonEmptySet[Position]] =
    board match {
      case x: Preflop => fromPreFlop(x)
      case x: Flop    => fromFlop(x)
      case x: Turn    => fromTurn(x)
      case x: River   => fromRiver(x)
    }

  def fromPreFlop(preflop: Preflop): Option[NonEmptySet[Position]] = {

    val flop  = Street.dealFlop(preflop)
    val turn  = Street.dealTurn(flop)
    val river = Street.dealRiver(turn)
    from(river)
  }

  def fromFlop(flop: Flop): Option[NonEmptySet[Position]] = {
    val turn  = Street.dealTurn(flop)
    val river = Street.dealRiver(turn)
    from(river)
  }

  def fromTurn(turn: Turn): Option[NonEmptySet[Position]] = {
    val river = Street.dealRiver(turn)
    from(river)
  }

  def fromRiver(river: River): Option[NonEmptySet[Position]] = {

    val hands: Seq[(Position, Hand)] = allHands(river)

    val positionSet: Set[Position] = hands
      .maximumByList[Hand](x => x._2)
      .map { case (playerPosition, _) => playerPosition }
      .toSet

    NonEmptySet.from(positionSet)
  }

  def allHands(board: River): List[(Position, Hand)] =
    board.players
      .map { case Player(pos, c1, c2) =>
        (
          pos,
          Hand.rank(
            List(
              c1,
              c2,
              board.card1,
              board.card2,
              board.card3,
              board.turn,
              board.river
            )
          )
        )
      }

}

object PlayerStanding {
  def apply(board: Street): List[(Int, Player, Hand)] =
    board.players
      .zip(board.allHands)
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
