package poker

case class Hand(
  cards: List[Card]
//  ranking: Ranking // TODO: Should this be optional?  Then ranked later?
  // Hands should not have rankings!  There should be a function from Hand to Ranking/Int
) {

  /// so really I could just sort the hand
  //  convert each val to hex decimal then
  // make a string and compare the hands
  def toScore: Score =
    Ranking(this) match {
      case HighCard => fromHighCard
    }

  private def fromHighCard: Score = {
    val sortedCards = cards.sortBy(_.rank.value).reverse.take(5)
    val vList2 = for {
      c <- sortedCards
      value = c.rank.value.toHexString
    } yield value
    val intValue = Integer.parseInt(vList2.mkString, 16)

//    val vList = for {
//      c <- sortedCards
//      value = c.rank.value
//    } yield value
//    val handTotal = vList.sorted.reverse.sum
//
//    println(sortedCards + " - " + Score(handTotal) + " - " + intValue)
    Score(intValue)

  }

  // add method
  // remove
}

case class Score(value: Int)
