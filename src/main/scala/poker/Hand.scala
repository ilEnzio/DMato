package poker

import cats.implicits._

import OrderInstances._

sealed trait Hand {
  def score: Int
}

object Hand {

  def rank(cards: List[Card]): Hand =
    cards match {
      case StraightFlush(x) => x
      case FourOfAKind(x)   => x
      case FullHouse(x)     => x
      case Flush(x)         => x
      case Straight(x)      => x
      case ThreeOfAKind(x)  => x
      case TwoPair(x)       => x
      case Pair(x)          => x
      case HighCard(x)      => x
    }

  final case class StraightFlush(rank: Rank) extends Hand {
    override val score = 9
  }

  final case class FourOfAKind(rank: Rank, kickers: List[Card]) extends Hand {
    override val score = 8
  }

  final case class FullHouse(rank1: Rank, rank2: Rank) extends Hand {
    override val score = 7
  }

  final case class Flush(rank: Rank, kickers: List[Card]) extends Hand {
    override val score = 6
  }

  final case class Straight(rank: Rank) extends Hand {
    override val score = 5
  }

  final case class ThreeOfAKind(rank: Rank, kickers: List[Card]) extends Hand {
    override val score = 4
  }

  final case class TwoPair(rank1: Rank, rank2: Rank, kicker: List[Card])
      extends Hand {
    override val score = 3
  }

  final case class Pair(rank: Rank, kickers: List[Card]) extends Hand {
    override val score = 2
  }

  final case class HighCard(rank: Rank, kickers: List[Card]) extends Hand {
    override val score = 1
  }

  // TODO use collectFirst or the appropriate HOF

  object StraightFlush {
    def unapply(hand: List[Card]): Option[StraightFlush] = {
      val suitMap = hand
        .groupBy(_.suit)
        .filter { case (_, cards) => cards.length >= 5 }

      if (suitMap.isEmpty) None
      else {
        val flushCards              = suitMap.toList.head._2.sorted.reverse
        val wheelStraight           = List(Ace, Five, Four, Three, Two)
        val rankOfStr: Option[Rank] = rankOfStraight(flushCards)
        if (rankOfStr.isDefined) Some(StraightFlush(rankOfStr.get))
        else if (flushCards.map(_.rank).count(wheelStraight.contains(_)) == 5)
          Some(StraightFlush(Five))
        else None
      }
    }
  }

  object FourOfAKind {
    def unapply(hand: List[Card]): Option[FourOfAKind] = {
      val rankGroups: List[(Rank, List[Card])] =
        hand.groupBy(_.rank).toList.sortBy(_._2.size).reverse
      rankGroups
        .collectFirst {
          case (rank, cards) if cards.length == 4 =>
            FourOfAKind(rank, rankGroups.tail.flatMap(_._2))
        }
    }
  }

  object FullHouse {
    def unapply(hand: List[Card]): Option[FullHouse] = {
      val rankGroups: Seq[(Rank, List[Card])] =
        hand.groupBy(_.rank).toList

      // sort; collect the first set
      // remove that set and repeat
      for {
        (rank1, set1) <- rankGroups.sortBy(_._1).findLast { case (_, cards) =>
          cards.size == 3
        }
        remain = hand.filterNot(set1.contains(_))
        (rank2, _) <- remain
          .groupBy(_.rank)
          .toList
          .sortBy(_._1)
          .find(_._2.size >= 2)
      } yield FullHouse(rank1, rank2)
    }
  }

  object Flush {
    def unapply(hand: List[Card]): Option[Flush] = {

      val groupBySuit5Count = hand
        .groupBy(_.suit)
        .toList
        .find { case (_, cards) =>
          cards.size >= 5
        }

      groupBySuit5Count.collectFirst { case (_, cards) =>
        Flush(cards.max.rank, cards.sorted.reverse)
      }
    }
  }

  object Straight {
    def unapply(hand: List[Card]): Option[Straight] = {
      val sorted                  = hand.distinctBy(_.rank).sortBy(_.rank).reverse
      val wheelStraight           = List(Ace, Five, Four, Three, Two)
      val rankOfStr: Option[Rank] = rankOfStraight(sorted)
      if (rankOfStr.isDefined) Some(Straight(rankOfStr.get))
      else if (sorted.map(_.rank).count(wheelStraight.contains(_)) == 5)
        Some(Straight(Five))
      else None
    }
  }

  private def rankOfStraight(sorted: List[Card]): Option[Rank] = {
    val strPair = sorted.zip(sorted.drop(4))
    strPair
      .find(x => x._1.rank.value == x._2.rank.value + 4)
      .map(_._1.rank)
  }

  object ThreeOfAKind {
    def unapply(hand: List[Card]): Option[ThreeOfAKind] = {

      val setGroup =
        hand.groupBy(_.rank).toList.find { case (_, cards) => cards.size == 3 }

      setGroup.collectFirst { case (rank, cards) =>
        val unusedCards = hand.filterNot(cards.contains(_)).sorted
        ThreeOfAKind(rank, unusedCards)
      }
    }
  }

  object TwoPair {
    def unapply(hand: List[Card]): Option[TwoPair] = {

      val pairsGrouped: Seq[(Rank, List[Card])] = hand
        .groupBy(_.rank)
        .filter(_._2.size == 2)
        .toList
        .sortBy(_._1.value)
        .reverse

      for {
        head <- pairsGrouped.headOption
        rest <- pairsGrouped.tail.headOption
        usedCards   = head._2 ++ rest._2
        unUsedCards = hand.filterNot(usedCards.contains(_)).sorted.reverse
      } yield TwoPair(head._1, rest._1, unUsedCards)
    }
  }

  object Pair {
    def unapply(hand: List[Card]): Option[Pair] = {

      val pair = hand
        .groupBy(_.rank)
        .filter { case (_, cards) => cards.size == 2 }
        .toList
      // pair.headOption.map{h => ...}
      pair match {
        case head :: Nil =>
          val usedCards   = head._2
          val unUsedCards = hand.filterNot(usedCards.contains(_)).sorted.reverse
          Some(Pair(pair.head._1, unUsedCards))

        case _ => None
      }
    }
  }

  object HighCard {
    def unapply(hand: List[Card]): Option[HighCard] = {
      val sorted = hand.sorted.reverse
      sorted match {
        case head :: _ => Some(HighCard(head.rank, sorted.tail))
        case _         => None
      }
    }
  }

}
