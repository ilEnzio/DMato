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

  implicit val handOrder: Order[Hand] = new Order[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      val xSorted = x.cards.sorted.reverse
      val ySorted = y.cards.sorted.reverse
      xSorted.zip(ySorted).foldLeft(0) { (s, v) =>
        s match {
          case 0          => cardOrder.compare(v._1, v._2)
          case x if x < 0 => -1
          case x if x > 0 => 1
        }
      }
    }
  }
  implicit val handOrdering = handOrder.toOrdering

}
