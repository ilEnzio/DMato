package poker

import cats.Order

object OrderInstances {

  implicit val rankingOrder: Order[Ranking] = new Order[Ranking] {
    override def compare(x: Ranking, y: Ranking): Int = x.score - y.score
  }

  implicit val rankingOrdering = rankingOrder.toOrdering

  implicit val cardOrder: Order[Card] = (x: Card, y: Card) => x.rank.value - y.rank.value

  implicit val cardOrdering = cardOrder.toOrdering // TODO ask about this.

  implicit val rankOrder: Order[Rank] = new Order[Rank] {
    override def compare(x: Rank, y: Rank): Int = x.value - y.value
  }

  implicit val rankOrdering = rankOrder.toOrdering
}
