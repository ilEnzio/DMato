//package poker
//import OrderInstances._
//import cats.Order
//
//case class ShowDown()
//
//object ShowDown {
//  def apply(hands: List[Hand]): List[Hand] = {
//
//    val grouped = hands
//      .groupBy(HandRank(_))
//      .toList
//      .sortBy(_._1)
//      .reverse
//
//    val (rankCategory, bHands) = grouped.head
//
//    rankCategory match {
//      case StraightFlush => evaluateStraightFlush(bHands)
//      case FourOfAKind   => evaluateFourOfAKind(bHands)
//      case FullHouse     => evaluateFullHouse(bHands)
//      case Flush         => evaluateFlush(bHands)
//      case Straight      => evaluateStraight(bHands)
//      case ThreeOfAKind  => evaluateThreeOfKind(bHands)
//      case TwoPair       => evaluateTwoPair(bHands)
//      case Pair          => evaluatePairs(bHands)
//      case _             => evaluateHighCard(bHands)
//
//    }
//  }
//
//  private def evaluateWinningHands(hands: List[Hand], order: Order[Hand]): List[Hand] =
//    hands.foldLeft(List.empty[Hand]) { (s, v) =>
//      if (order.compare(v, hands.head) == 0) v :: s
//      else s
//    }
//
//  private def evaluateHighCard(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted.reverse
//    evaluateWinningHands(ordered, handOrder)
//  }
//
//  private def evaluatePairs(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(pairOrdering).reverse
//    evaluateWinningHands(ordered, pairOrder)
//  }
//
//  private def evaluateTwoPair(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(twoPairOrdering).reverse
//    evaluateWinningHands(ordered, twoPairOrder)
//  }
//
//  private def evaluateThreeOfKind(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(threeOfAKindOrdering).reverse
//    evaluateWinningHands(ordered, threeOfAKindOrder)
//  }
//
//  private def evaluateStraight(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(straightOrdering).reverse
//    evaluateWinningHands(ordered, straightOrder)
//  }
//
//  private def evaluateFlush(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(flushOrdering).reverse
//    evaluateWinningHands(ordered, flushOrder)
//  }
//
//  private def evaluateFullHouse(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(fullHouseOrdering).reverse
//    evaluateWinningHands(ordered, fullHouseOrder)
//  }
//
//  private def evaluateFourOfAKind(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(fourOfAKindOrdering).reverse
//    evaluateWinningHands(ordered, fourOfAKindOrder)
//  }
//
//  private def evaluateStraightFlush(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(straightFlushOrdering).reverse
//    evaluateWinningHands(ordered, straightFlushOrder)
//  }
//}
