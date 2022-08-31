package poker

import cats.Order
import poker.Rank.rankMap

import scala.annotation.tailrec

object OrderInstances {
///  TODO: General, this is wrong, because they are all order of Hand
  // I should have only one Order instance of hand
  // therefore what I could do is model
  implicit val handRankingOrder: Order[HandRank] = new Order[HandRank] {
    override def compare(x: HandRank, y: HandRank): Int = x.score - y.score
  }
  implicit val handRankingOrdering = handRankingOrder.toOrdering

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

  val twoPairOrder: Order[Hand] = new Order[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      val xGrouped = x.cards.groupBy(c => c.rank).toList.sortBy(_._2.size).reverse
      val yGrouped = y.cards.groupBy(c => c.rank).toList.sortBy(_._2.size).reverse
      val xFinal   = xGrouped.take(2).flatMap(_._2) ++ xGrouped.drop(2).flatMap(_._2)
      val yFinal   = yGrouped.take(2).flatMap(_._2) ++ yGrouped.drop(2).flatMap(_._2)
      compareCorresponding(xFinal, yFinal)
    }
  }

  val twoPairOrdering = twoPairOrder.toOrdering

  val threeOfAKindOrder: Order[Hand] = new Order[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      val xGrouped = x.cards.groupBy(c => c.rank).toList.sortBy(_._2.size).reverse
      val yGrouped = y.cards.groupBy(c => c.rank).toList.sortBy(_._2.size).reverse
      val xFinal   = xGrouped.head._2 ++ xGrouped.tail.flatMap(_._2).sorted.reverse.take(2)
      val yFinal   = yGrouped.head._2 ++ yGrouped.tail.flatMap(_._2).sorted.reverse.take(2)
      compareCorresponding(xFinal, yFinal)

    }
  }

  val threeOfAKindOrdering = threeOfAKindOrder.toOrdering

  val straightOrder: Order[Hand] = new Order[Hand] {
    val wheelStraight = List(Ace, Five, Four, Three, Two)
    override def compare(x: Hand, y: Hand): Int = {
      val xSorted: List[Rank] = x.cards.map(_.rank).distinct.sorted.reverse
      val ySorted: List[Rank] = y.cards.map(_.rank).distinct.sorted.reverse

      def wheelCheck(sorted: List[Rank]): Rank =
        sorted.filter(r => wheelStraight.contains(r)).size == 5 match {
          case true =>
            rankMap(Integer.max(check4Str(sorted).value, 5))
          case false => check4Str(sorted)
        }

      @tailrec
      def check4Str(cards: List[Rank]): Rank =
        if (cards.length < 5) Two /// This is Wrong
        else if (cards.head.value == cards(4).value + 4) cards.head
        else check4Str(cards.drop(1))

      wheelCheck(xSorted).value - wheelCheck(ySorted).value
    }
  }

  val straightOrdering = straightOrder.toOrdering

  val flushOrder: Order[Hand] = new Order[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      val xFlushCards = x.cards.groupBy(_.suit).filter(_._2.size >= 5).head._2
      val yFlushCards = y.cards.groupBy(_.suit).filter(_._2.size >= 5).head._2
      val xFinal      = xFlushCards.map(_.rank).sorted.reverse.head
      val yFinal      = yFlushCards.map(_.rank).sorted.reverse.head

      xFinal.value - yFinal.value
    }
  }
  val flushOrdering = flushOrder.toOrdering

  val fullHouseOrder: Order[Hand] = new Order[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      val xGrouped = x.cards.groupBy(_.rank).toList.sortBy(_._2.size).reverse
      val yGrouped = y.cards.groupBy(_.rank).toList.sortBy(_._2.size).reverse
      val xRank    = xGrouped.head._1
      val yRank    = yGrouped.head._1
      if (xRank.value - yRank.value != 0) xRank.value - yRank.value // TODO I basically need this for everything!
      else {
        val xRank2 = xGrouped
          .filter(_._2.size == 2)
          .sortBy(_._1)
          .reverse
          .head
          ._1
        val yRank2 = yGrouped
          .filter(_._2.size == 2)
          .sortBy(_._1)
          .reverse
          .head
          ._1
        if (xRank2.value - yRank2.value != 0) xRank2.value - yRank2.value
        else xRank2.value - yRank2.value // TODO WRong!!!!
      }
    }
  }
  val fullHouseOrdering = fullHouseOrder.toOrdering

  val fourOfAKindOrder: Order[Hand] = new Order[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      val xGrouped = x.cards.groupBy(_.rank).toList.sortBy(_._2.size).reverse
      val yGrouped = y.cards.groupBy(_.rank).toList.sortBy(_._2.size).reverse
      val xRank    = xGrouped.head._1
      val yRank    = yGrouped.head._1
      xRank.value - yRank.value
    }
  }

  val fourOfAKindOrdering = fourOfAKindOrder.toOrdering

  val straightFlushOrder: Order[Hand] = new Order[Hand] {
    override def compare(x: Hand, y: Hand): Int =
      straightOrder.compare(x, y)
  }

  val straightFlushOrdering = straightFlushOrder.toOrdering
}
