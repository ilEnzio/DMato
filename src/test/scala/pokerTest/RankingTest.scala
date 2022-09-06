package pokerTest

import org.scalacheck.Prop.{all, forAll, propBoolean, AnyOperators}
import org.scalacheck.{Properties}
import poker.Hand_2._
import pokerData.HandGenerators._
import poker._

object RankingTest extends Properties("Ranking Tests") {

  property("A StraightFlush is not Ranked a Straight or Flush ") = forAll(genStraightFlush) { hand =>
    (Hand_2.rank(hand.cards) match {
      case _: Straight      => false
      case _: StraightFlush => true
      case _                => false
    }) :| "Not Ranked Straight" &&
    (Hand_2.rank(hand.cards) match {
      case _: Flush         => false
      case _: StraightFlush => true
      case _                => false
    }) :| "Not Ranked Flush"
  }

  property("FourOfAKind must have 4 cards of same rank") = forAll(genFourOfAKind) { hand =>
    hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
      cards.length == 4
    }
  }

  property("a hand is 4 of a kind is FourOfAKind and NOT ThreeOfAKind") = forAll(genFourOfAKind) { hand =>
//    (HandRank(hand) == FourOfAKind) ++ (HandRank(hand) != ThreeOfAKind)
    (Hand_2.rank(hand.cards) match {
      case _: ThreeOfAKind => false
      case _: FourOfAKind  => true
      case _               => false
    }) :| "Not ThreeOfAKind"
  }

  property("a FullHouse has 4 or less ranks") = forAll(genFullHouse) { hand =>
    hand.cards.groupBy(_.rank).size <= 4
  }

  property("5 or more cards of a suit is a Flush") = forAll(genFlush, genNonFlush) { (flushHand, nonFlushHand) =>
    (Hand_2.rank(flushHand.cards) match {
      case _: StraightFlush => false
      case _                => true
    }) :| "Constraint" ==> {
      (Hand_2.rank(flushHand.cards) match {
        case _: Flush => true
        case _        => false
      }) :| "Flush" &&
      (Hand_2.rank(nonFlushHand.cards) match {
        case _: Flush => false
        case _        => true
      }) :| "Non-Flush"
    }
  }

  property("a Flush hand has 3 or less suits") = forAll(genFlush) { hand =>
    hand.cards.groupBy(c => c.suit).size <= 3
  }

  property("sequential cards rank a Straight") = forAll(genStraight) { hand =>
    all(
      Hand_2.rank(hand.cards) match {
        case _: StraightFlush => false
        case _                => true
      },
      Hand_2.rank(hand.cards) match {
        case _: Flush => false
        case _        => true
      }
    ) ==> {
      Hand_2.rank(hand.cards) match {
        case _: Straight => true
        case _           => false
      }
    }
  }

  property("3 cards of the same rank is ThreeOfAKind") = forAll(genThreeOfAKind) { hand =>
    Hand_2.rank(hand.cards) match {
      case _: ThreeOfAKind => true
      case _               => false
    }
  }

  property("ThreeOfAKind has 5 ranks and is NOT a Straight") = forAll(genThreeOfAKind) { hand =>
    (hand.cards.groupBy(_.rank).size ?= 5) &&
    (Hand_2.rank(hand.cards) match {
      case _: Straight => false
      case _           => true
    })
  }

  property("at least 2 pair of cards in a hand is TwoPair") = forAll(genTwoPair) { hand =>
    (Hand_2.rank(hand.cards) match {
      case _: StraightFlush | _: Flush | _: Straight => false
      case _                                         => true
    }) ==> {
      Hand_2.rank(hand.cards) match {
        case _: TwoPair => true
        case _          => false
      }
    }
  }

  property("at least two cards of the same rank is a Pair") = forAll(genPair) { hand =>
    Hand_2.rank(hand.cards) match {
      case _: Pair => true
      case _       => false
    }
  }

  property("A HighCard hand has no other rank") = forAll(genHighCard) { hand =>
    Hand_2.rank(hand.cards) match {
      case _: HighCard => true
      case _           => false
    }
  }

}
