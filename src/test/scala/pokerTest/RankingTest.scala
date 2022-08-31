package pokerTest

import org.scalacheck.Prop.{all, forAll, propBoolean, AnyOperators}
import org.scalacheck.{Arbitrary, Gen, Properties}
import poker.{Flush, FourOfAKind, Hand, HighCard, Pair, Straight, StraightFlush, ThreeOfAKind, TwoPair}
import test.pokerData.DataGenerators._
import poker._

object RankingTest extends Properties("Ranking Tests") {

  // a poker hand is 5 cards.  but I think for the purposes of this
  // it is the best 5 cards of 7, and the rank of the hand is determined by
  // the five cards

  property("A StraightFlush is not Ranked a Straight or Flush ") = forAll(genStraightFlush) { hand =>
    (HandRank(hand) != Straight) :| "Not Ranked Straight" &&
    (HandRank(hand) != Flush) :| "Not Ranked Flush" &&
    (HandRank(hand) ?= StraightFlush) :| "Ranked a StraightFlush"
  }

  property("FourOfAKind must have 4 cards of same rank") = forAll(genFourOfAKind) { hand =>
    hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
      cards.length == 4
    }
  }

  property("a hand is 4 of a kind is FourOfAKind and NOT ThreeOfAKind") = forAll(genFourOfAKind) { hand =>
    (HandRank(hand) == FourOfAKind) ++ (HandRank(hand) != ThreeOfAKind)
  }

  property("a FullHouse has 4 or less ranks") = forAll(genFullHouse) { hand: Hand =>
    hand.cards.groupBy(_.rank).size <= 4
  }

  property("5 or more cards of a suit is a Flush") = forAll(genFlush, genNonFlush) { (flushHand, nonFlushHand) =>
    (HandRank(flushHand) != StraightFlush) :| "Constraint" ==> {
      (HandRank(flushHand) ?= Flush) :| "Flush" && (HandRank(nonFlushHand) != Flush) :| "Non-Flush"
    }
  }

  property("a Flush hand has 3 or less suits") = forAll(genFlush) { hand =>
    hand.cards.groupBy(c => c.suit).size <= 3
  }

  property("sequential cards rank a Straight") = forAll(genStraight) { hand =>
    all(HandRank(hand) != StraightFlush, HandRank(hand) != Flush) ==> {
      HandRank(hand) ?= Straight
    }
  }

  property("3 cards of the same rank is ThreeOfAKind") = forAll(genThreeOfAKind) { hand =>
    HandRank(hand) ?= ThreeOfAKind
  }

  property("ThreeOfAKind has 5 ranks and is NOT a Straight") = forAll(genThreeOfAKind) { hand =>
    (hand.cards.groupBy(_.rank).size ?= 5) &&
    HandRank(hand) != Straight
  }

  property("at least 2 pair of cards in a hand is TwoPair") = forAll(genTwoPair) { hand =>
    all(HandRank(hand) != StraightFlush, HandRank(hand) != Flush, HandRank(hand) != Straight) ==> {
      HandRank(hand) ?= TwoPair
    }
  }

  property("at least two cards of the same rank is a Pair") = forAll(genPair) { hand =>
    HandRank(hand) ?= Pair
  }

  property("A HighCard hand has no other rank") = forAll(genHighCard) { hand =>
    (HandRank(hand) ?= HighCard)
  }

}
