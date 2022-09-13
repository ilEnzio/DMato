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
        (x.cards, y.cards) match {
          case (StraightFlush(a), StraightFlush(b)) => a.rank.value - b.rank.value
          case (FourOfAKind(a), FourOfAKind(b)) =>
            if (a.rank.value - b.rank.value != 0) a.rank.value - b.rank.value
            else compareCorresponding(a.kickers, b.kickers)
          case (FullHouse(a), FullHouse(b)) =>
            if (a.rank1.value - b.rank1.value != 0) a.rank1.value - b.rank1.value
            else a.rank2.value - b.rank2.value
          case (Flush(a), Flush(b))       => a.rank.value - b.rank.value
          case (Straight(a), Straight(b)) => a.rank.value - b.rank.value
          case (ThreeOfAKind(a), ThreeOfAKind(b)) =>
            if (a.rank.value - b.rank.value != 0) a.rank.value - b.rank.value
            else compareCorresponding(a.kickers, b.kickers)
          case (TwoPair(a), TwoPair(b)) =>
            if (a.rank1.value - b.rank1.value != 0) a.rank1.value - a.rank1.value
            else if (a.rank2.value - b.rank2.value != 0) a.rank2.value - b.rank2.value
            else compareCorresponding(a.kicker, b.kicker)
          case (Pair(a), Pair(b)) =>
            if (a.rank.value - b.rank.value != 0) a.rank.value - b.rank.value
            else compareCorresponding(a.kickers, b.kickers)
          case (HighCard(a), HighCard(b)) => compareCorresponding(a.cards, b.cards)
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
