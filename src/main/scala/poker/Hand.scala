package poker

import cats.Order
import cats.instances.all

case class Hand(
  cards: List[Card]
) {
  /// so really I could just sort the hand
  //  convert each val to hex decimal then
  // make a string and compare the hands
  def toScore: Score =
    Ranking(this) match {
      case Pair => fromPair
      case _    => fromHighCard
    }

  private def fromPair: Score = {
    // groupby; filter
    // add 1,000,000 for every rank of pair
    val (pair, other) = cards
      .groupBy(_.rank)
      .toList
      .partition(_._2.size == 2)

    val pairScore = (pair(0)._1.value * 1000000).toLong
    val otherCards = other
      .flatMap { case (_, cards) =>
        cards
      }
      .sortBy(_.rank.value)
      .reverse
      .take(3)
    val otherScore = Hand(otherCards).fromHighCard.value

//    println(pair(0)._2 ++ otherCards + " - " + (pairScore + otherScore))
    Score(pairScore + otherScore)
  }

  private def fromHighCard: Score = {
    val sortedCards = cards.sortBy(_.rank.value).reverse.take(5)
    val vList2 = for {
      c <- sortedCards
      value = c.rank.value.toHexString
    } yield value
    val intValue = Integer.parseInt(vList2.mkString, 16).toLong

    Score(intValue)
  }
  // add method
  // remove
}

case class Score(value: Long)
