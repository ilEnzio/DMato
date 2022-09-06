package poker

import cats.implicits._

//import cats.syntax.all._
import OrderInstances._

sealed trait Hand_2 {
  val cards: List[Card]
  val score = 0
}

object Hand_2 {
  // ToDo: Why doesn't this work anymore?
  //  val allRanks: List[UnrankedHand] =
  //    List(StraightFlush, FourOfAKind, FullHouse, Flush, Straight, ThreeOfAKind, TwoPair, Pair, HighCard)

  def rank(cards: List[Card]): Hand_2 =
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

  final case class UnrankedHand(cards: List[Card]) extends Hand_2

  final case class StraightFlush(cards: List[Card], rank: Rank) extends Hand_2 {
    override val score = 9
  }

  final case class FourOfAKind(cards: List[Card], rank: Rank, kickers: List[Card]) extends Hand_2 {
    override val score = 8
    //override   override val cards: List[Card] = ???
  }

  final case class FullHouse(cards: List[Card], rank1: Rank, rank2: Rank) extends Hand_2 {
    override val score = 7
  }

  final case class Flush(cards: List[Card], rank: Rank) extends Hand_2 {
    override val score = 6
  }

  final case class Straight(cards: List[Card], rank: Rank) extends Hand_2 {
    override val score = 5
  }

  final case class ThreeOfAKind(cards: List[Card], rank: Rank, kickers: List[Card]) extends Hand_2 {
    override val score = 4
  }

  final case class TwoPair(cards: List[Card], rank1: Rank, rank2: Rank, kicker: List[Card]) extends Hand_2 {
    override val score = 3
  }

  final case class Pair(cards: List[Card], rank: Rank, kickers: List[Card]) extends Hand_2 {
    override val score = 2
  }

  final case class HighCard(cards: List[Card], rank: Rank) extends Hand_2 {
    override val score = 1
  }

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
        .exists { case (_, cards) => cards.length == 4 }
        .guard[Option]
        .as {
          FourOfAKind(hand, rankGroups.head._1, rankGroups.tail.flatMap(_._2))
        }
    }
  }

  object FullHouse {
    def unapply(hand: List[Card]): Option[FullHouse] = {
      val rankGroups: Seq[(Rank, List[Card])] = hand.groupBy(_.rank).toList.sortBy(_._2.size).reverse
      val groupsFlattened                     = rankGroups.flatMap(_._2)
      val count                               = rankGroups.map(_._2.size)

      // first list zip with second list
//      val rankSizeGroup = rankGroups.map{x => (x._1, x._2.size)}

      ((count.size > 2) && count.take(2).sum >= 5)
        .guard[Option]
        .as {
          FullHouse(hand, groupsFlattened.head.rank, groupsFlattened(3).rank)
        }

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

      groupBySuit5Count.isDefined
        .guard[Option]
        .as {
          for {
            cards <- groupBySuit5Count._2F
            rank = cards.sorted.reverse.head.rank
          } yield Flush(hand, rank)
        }
        .flatten
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

      setGroup.isDefined
        .guard[Option]
        .as {
          for {
            setCards    <- setGroup._2F
            rank        <- setGroup._1F
            unusedCards <- Some(hand.filterNot(setCards.contains(_)).sorted)
          } yield ThreeOfAKind(hand, rank, unusedCards)
        }
        .flatten
    }
  }

  object TwoPair {
    def unapply(hand: List[Card]): Option[TwoPair] = {

      val pairsGrouped: Seq[(Rank, List[Card])] = hand
        .groupBy(_.rank)
        .filter(_._2.size == 2)
        .toList
        .sortBy(_._1.value)

      (pairsGrouped.size > 1)
        .guard[Option]
        .as {
          for {
            head <- pairsGrouped.headOption
            rest <- pairsGrouped.tail.headOption
            usedCards   = head._2 ++ rest._2
            unUsedCards = hand.filterNot(usedCards.contains(_)).sorted.reverse
          } yield TwoPair(hand, head._1, rest._1, unUsedCards)
        }
        .flatten
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
