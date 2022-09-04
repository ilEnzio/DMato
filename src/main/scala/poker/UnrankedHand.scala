package poker

import cats._
import cats.syntax.all._
import OrderInstances._
sealed trait UnrankedHand {
  val cards: List[Card]
  val score = 0
}

object UnrankedHand {
  // ToDo: Why doesn't this work anymore?
  //  val allRanks: List[UnrankedHand] =
  //    List(StraightFlush, FourOfAKind, FullHouse, Flush, Straight, ThreeOfAKind, TwoPair, Pair, HighCard)

  def rankHand(hand: UnrankedHand): UnrankedHand =
    hand match {
      case StraightFlush(cards, rank)            => StraightFlush(cards, rank)
      case FourOfAKind(cards, rank, kickers)     => FourOfAKind(cards, rank, kickers)
      case FullHouse(cards, rank1, rank2)        => FullHouse(cards, rank1, rank2)
      case Flush(cards, rank)                    => Flush(cards, rank)
      case Straight(cards, rank)                 => Straight(cards, rank)
      case ThreeOfAKind(cards, rank, kickers)    => ThreeOfAKind(cards, rank, kickers)
      case TwoPair(cards, rank1, rank2, kickers) => TwoPair(cards, rank1, rank2, kickers)
      case Pair(cards, rank, kickers)            => Pair(cards, rank, kickers)
      case HighCard(cards, rank)                 => HighCard(cards, rank)
    }

  final case class StraightFlush(cards: List[Card], rank: Rank) extends UnrankedHand {
    override val score = 9
  }

  final case class FourOfAKind(cards: List[Card], rank: Rank, kickers: List[Card]) extends UnrankedHand {
    override val score = 8
    //override   override val cards: List[Card] = ???
  }

  final case class FullHouse(cards: List[Card], rank1: Rank, rank2: Rank) extends UnrankedHand {
    override val score = 7
  }

  final case class Flush(cards: List[Card], rank: Rank) extends UnrankedHand {
    override val score = 6
  }

  final case class Straight(cards: List[Card], rank: Rank) extends UnrankedHand {
    override val score = 5
  }

  final case class ThreeOfAKind(cards: List[Card], rank: Rank, kickers: List[Card]) extends UnrankedHand {
    override val score = 4
  }

  final case class TwoPair(cards: List[Card], rank1: Rank, rank2: Rank, kicker: List[Card]) extends UnrankedHand {
    override val score = 3
  }

  final case class Pair(cards: List[Card], rank: Rank, kickers: List[Card]) extends UnrankedHand {
    override val score = 2
  }

  final case class HighCard(cards: List[Card], rank: Rank) extends UnrankedHand {
    override val score = 1
  }

  object StraightFlush {
    def unapply(hand: UnrankedHand): Option[(List[Card], Rank)] = {
      val suitMap = hand.cards
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
        if (check4Str.isDefined) Some(hand.cards, check4Str.get)
        else if (flushCards.count(wheelStraight.contains(_)) == 5) Some(hand.cards, Five)
        else None
      }
    }
  }

  object FourOfAKind {
    def unapply(hand: UnrankedHand): Option[(List[Card], Rank, List[Card])] = {
      val rankGroups = hand.cards.groupBy(_.rank).toList.sortBy(_._2.size).reverse
      rankGroups
        .exists { case (_, cards) => cards.length == 4 }
        .guard[Option]
        .as {
          (hand.cards, rankGroups.head._1, rankGroups.tail.flatMap(_._2))
        }
    }
  }

  object FullHouse {
    def unapply(hand: UnrankedHand): Option[(List[Card], Rank, Rank)] = {
      val rankGroups      = hand.cards.groupBy(_.rank).toList.sortBy(_._2.size).reverse
      val groupsFlattened = rankGroups.flatMap(_._2)
      val count           = rankGroups.map(_._2.size)
      ((count.size < 2) && count.take(2).sum >= 5)
        .guard[Option]
        .as {
          (hand.cards, groupsFlattened.head.rank, groupsFlattened(3).rank)
        }

    }
  }

  object Flush {
    def unapply(hand: UnrankedHand): Option[(List[Card], Rank)] = {
      val flushCards = hand.cards
        .groupBy(_.suit)
        .filter(_._2.size >= 5)
        .toList
        .flatMap(_._2)
        .sorted
        .reverse
      (flushCards != Nil)
        .guard[Option]
        .as(
          hand.cards,
          flushCards.head.rank
        )
    }
  }

  object Straight {
    def unapply(hand: UnrankedHand): Option[(List[Card], Rank)] = {
      val sorted        = hand.cards.distinctBy(_.rank).sortBy(c => c.rank).reverse
      val wheelStraight = List(Ace, Five, Four, Three, Two)
      val check4Str: Option[Rank] = {
        val strPair = sorted.zip(sorted.drop(4))
        strPair
          .find(x => x._1.rank.value == x._2.rank.value + 4)
          .map(_._1.rank)
      }
      if (check4Str.isDefined) Some(hand.cards, check4Str.get)
      else if (sorted.count(wheelStraight.contains(_)) == 5) Some(hand.cards, Five)
      else None
    }
  }

  object ThreeOfAKind {
    def unapply(hand: UnrankedHand): Option[(List[Card], Rank, List[Card])] = {

      val setGroup = hand.cards.groupBy(_.rank).toList.find { case (_, cards) => cards.size == 3 }

      setGroup.isDefined
        .guard[Option]
        .as {
          for {
            setCards    <- setGroup._2F
            rank        <- setGroup._1F
            unusedCards <- Some(hand.cards.filterNot(setCards.contains(_)).sorted)
          } yield (hand.cards, rank, unusedCards)
        }
        .flatten
    }
  }

  object TwoPair {
    def unapply(hand: UnrankedHand): Option[(List[Card], Rank, Rank, List[Card])] = {

      val pairsGrouped: Seq[(Rank, List[Card])] = hand.cards
        .groupBy(_.rank)
        .filter { case (_, cards) => cards.size == 2 }
        .toList
        .sortBy(_._1.value)

      (pairsGrouped.size > 1).guard[Option].as {
        val usedCards   = pairsGrouped.head._2 ++ pairsGrouped(1)._2
        val unUsedCards = hand.cards.filterNot(usedCards.contains(_)).sorted.reverse
        (hand.cards, pairsGrouped(0)._1, pairsGrouped(1)._1, unUsedCards)

      }
    }
  }

  object Pair {
    def unapply(hand: UnrankedHand): Option[(List[Card], Rank, List[Card])] = {

      val pair = hand.cards
        .groupBy(_.rank)
        .filter { case (_, cards) => cards.size == 2 }
        .toList
        .sortBy(_._1.value)

      (pair.size == 1).guard[Option].as {
        val usedCards   = pair.head._2
        val unUsedCards = hand.cards.filterNot(usedCards.contains(_)).sorted.reverse
        (hand.cards, pair.head._1, unUsedCards)
      }
    }
  }
}
