package poker

import cats.Order
import poker.Hand._

object OrderInstances {
///  TODO: General, this is wrong, because they are all order of Hand
  // I should have only one Order instance of hand
  // therefore what I could do is model

  implicit val handOrder: Order[Hand] = new Order[Hand] {
    override def compare(x: Hand, y: Hand): Int =
      if (x.score != y.score) x.score - y.score
      else
        (x, y) match {
          case (a: StraightFlush, b: StraightFlush) => a.rank.value - b.rank.value
          case (a: FourOfAKind, b: FourOfAKind) =>
            if (a.rank.value - b.rank.value != 0) a.rank.value - b.rank.value
            else compareCorresponding(a.kickers, b.kickers)
          case (a: FullHouse, b: FullHouse) =>
            if (a.rank1.value - b.rank1.value != 0) a.rank1.value - b.rank1.value
            else a.rank2.value - b.rank2.value
          case (a: Flush, b: Flush)       => a.rank.value - b.rank.value
          case (a: Straight, b: Straight) => a.rank.value - b.rank.value
          case (a: ThreeOfAKind, b: ThreeOfAKind) =>
            if (a.rank.value - b.rank.value != 0) a.rank.value - b.rank.value
            else compareCorresponding(a.kickers, b.kickers)
          case (a: TwoPair, b: TwoPair) =>
            if (a.rank1.value - b.rank1.value != 0) a.rank1.value - a.rank1.value
            else if (a.rank2.value - b.rank2.value != 0) a.rank2.value - b.rank2.value
            else compareCorresponding(a.kicker, b.kicker)
          case (a: Pair, b: Pair) =>
            if (a.rank.value - b.rank.value != 0) a.rank.value - b.rank.value
            else compareCorresponding(a.kickers, b.kickers)
          case (a: HighCard, b: HighCard) => compareCorresponding(a.kickers, b.kickers)
        }
  }
  implicit val handOrdering: Ordering[Hand] = handOrder.toOrdering

  implicit val cardOrder: Order[Card] = (x: Card, y: Card) => x.rank.value - y.rank.value
  implicit val cardOrdering           = cardOrder.toOrdering // TODO ask about this.

  implicit val rankOrder: Order[Rank] = new Order[Rank] {
    override def compare(x: Rank, y: Rank): Int = x.value - y.value
  }
  implicit val rankOrdering = rankOrder.toOrdering

  private def compareCorresponding(sortedCards1: List[Card], sortedCards2: List[Card]): Int =
    sortedCards1.zip(sortedCards2).foldLeft(0) { (s, v) =>
      s match {
        case 0          => cardOrder.compare(v._1, v._2)
        case x if x < 0 => -1
        case x if x > 0 => 1
      }
    }

}
