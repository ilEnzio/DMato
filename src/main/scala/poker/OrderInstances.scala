package poker

import cats.Order

object OrderInstances {

  implicit val rankingOrder: Order[HandRank] = new Order[HandRank] {
    override def compare(x: HandRank, y: HandRank): Int = x.score - y.score
  }
  implicit val rankingOrdering = rankingOrder.toOrdering

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

  implicit val handOrder: Order[Hand] = new Order[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      val xSorted = x.cards.sorted.reverse
      val ySorted = y.cards.sorted.reverse
      compareCorresponding(xSorted, ySorted)
    }
  }
  implicit val handOrdering = handOrder.toOrdering

  val pairOrder: Order[Hand] = new Order[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      // split out the pair
      // then sort the remaining cards; reversed
      // concat; take 5
      // compare the rest like a normal hand?
      val xGrouped = x.cards.groupBy(c => c.rank).toList.sortBy(_._2.size).reverse
      val yGrouped = y.cards.groupBy(c => c.rank).toList.sortBy(_._2.size).reverse
      val xFinal   = xGrouped.head._2 ++ xGrouped.tail.flatMap(_._2).sorted.reverse.take(3)
      val yFinal   = yGrouped.head._2 ++ yGrouped.tail.flatMap(_._2).sorted.reverse.take(3)
      compareCorresponding(xFinal, yFinal)
    }
  }

  val pairOrdering = pairOrder.toOrdering
}
