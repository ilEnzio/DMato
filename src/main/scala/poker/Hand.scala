package poker

import cats.implicits._

//import cats.syntax.all._
import OrderInstances._

sealed trait Hand {
  val cards: List[Card]
  val score = 0 // do I need this?
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

  final case class UnrankedHand(cards: List[Card]) extends Hand

  final case class StraightFlush(cards: List[Card], rank: Rank) extends Hand {
    override val score = 9
  }

  final case class FourOfAKind(cards: List[Card], rank: Rank, kickers: List[Card]) extends Hand {
    override val score = 8
  }

  final case class FullHouse(cards: List[Card], rank1: Rank, rank2: Rank) extends Hand {
    override val score = 7
  }

  final case class Flush(cards: List[Card], rank: Rank) extends Hand {
    override val score = 6
  }

  final case class Straight(cards: List[Card], rank: Rank) extends Hand {
    override val score = 5
  }

  final case class ThreeOfAKind(cards: List[Card], rank: Rank, kickers: List[Card]) extends Hand {
    override val score = 4
  }

  final case class TwoPair(cards: List[Card], rank1: Rank, rank2: Rank, kicker: List[Card]) extends Hand {
    override val score = 3
  }

  final case class Pair(cards: List[Card], rank: Rank, kickers: List[Card]) extends Hand {
    override val score = 2
  }

  final case class HighCard(cards: List[Card], rank: Rank) extends Hand {
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
        val flushCards    = suitMap.toList.head._2.sorted.reverse
        val wheelStraight = List(Ace, Five, Four, Three, Two)
        val check4Str: Option[Rank] = {
          val strPair = flushCards.zip(flushCards.drop(4))
          strPair
            .find(x => x._1.rank.value == x._2.rank.value + 4)
            .map(_._1.rank)
        }
        if (check4Str.isDefined) Some(StraightFlush(hand, check4Str.get))
        else if (flushCards.map(_.rank).count(wheelStraight.contains(_)) == 5) Some(StraightFlush(hand, Five))
        else None
      }
    }
  }

  object FourOfAKind {
    def unapply(hand: List[Card]): Option[FourOfAKind] = {
      val rankGroups = hand.groupBy(_.rank).toList.sortBy(_._2.size).reverse
      rankGroups
        .collectFirst {
          case (rank, cards) if cards.length == 4 =>
            FourOfAKind(hand, rank, rankGroups.tail.flatMap(_._2))
        }
    }
  }

  object FullHouse {
    def unapply(hand: List[Card]): Option[FullHouse] = {
      val rankGroups: Seq[(Rank, List[Card])] = hand.groupBy(_.rank).toList.sortBy(_._2.size).reverse

      // sort; collect the first set
      // remove that set and repeat
      for {
        (rank1, set1) <- rankGroups.find { case (_, cards) =>
          cards.size == 3
        }
        remain = hand.filterNot(set1.contains(_))
        (rank2, _) <- remain
          .groupBy(_.rank)
          .toList
          .sortBy(_._1)
          .findLast(_._2.size >= 2)
      } yield FullHouse(hand, rank1, rank2)
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
        Flush(hand, cards.max.rank)
      }
    }
  }

  object Straight {
    def unapply(hand: List[Card]): Option[Straight] = {
      val sorted        = hand.distinctBy(_.rank).sortBy(_.rank).reverse
      val wheelStraight = List(Ace, Five, Four, Three, Two)
      val check4Str: Option[Rank] = {
        val strPair = sorted.zip(sorted.drop(4))
        strPair
          .find(x => x._1.rank.value == x._2.rank.value + 4)
          .map(_._1.rank)
      }
      if (check4Str.isDefined) Some(Straight(hand, check4Str.get))
      else if (sorted.map(_.rank).count(wheelStraight.contains(_)) == 5) Some(Straight(hand, Five))
      else None
    }
  }

  object ThreeOfAKind {
    def unapply(hand: List[Card]): Option[ThreeOfAKind] = {

      val setGroup = hand.groupBy(_.rank).toList.find { case (_, cards) => cards.size == 3 }

      setGroup.collectFirst { case (rank, cards) =>
        val unusedCards = hand.filterNot(cards.contains(_)).sorted
        ThreeOfAKind(hand, rank, unusedCards)
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

      for {
        head <- pairsGrouped.headOption
        rest <- pairsGrouped.tail.headOption
        usedCards   = head._2 ++ rest._2
        unUsedCards = hand.filterNot(usedCards.contains(_)).sorted.reverse
      } yield TwoPair(hand, head._1, rest._1, unUsedCards)
    }
  }

  object Pair {
    def unapply(hand: List[Card]): Option[Pair] = {

      val pair = hand
        .groupBy(_.rank)
        .filter { case (_, cards) => cards.size == 2 }
        .toList
        .sortBy(_._1.value)

      (pair.size == 1)
        .guard[Option]
        .as {
          for {
            head <- pair.headOption
            usedCards   = head._2
            unUsedCards = hand.filterNot(usedCards.contains(_)).sorted.reverse
          } yield Pair(hand, pair.head._1, unUsedCards)
        }
        .flatten
    }
  }
  object HighCard {
    def unapply(hand: List[Card]): Option[HighCard] = {
      val sorted = hand.sorted.reverse
      val rank   = sorted.head.rank
      Some(HighCard(sorted, rank))
    }
  }
}
