package poker

import cats.implicits.catsSyntaxPartialOrder
import org.scalacheck.Prop.{all, forAll, propBoolean, AnyOperators}
import org.scalacheck.{Arbitrary, Gen, Properties}
import poker.OrderInstances.rankOrder
import project.DataGenerators._

object RankingTest extends Properties("RankingTest") {

  // a poker hand is 5 cards.  but I think for the purposes of this
  // it is the best 5 cards of 7, and the rank of the hand is determined by
  // the five cards

  property("A StraightFlush is not Ranked a Straight or Flush ") = forAll(genStraightFlush) { hand =>
    (Ranking(hand) != Straight) :| "Not Ranked Straight" &&
    (Ranking(hand) != Flush) :| "Not Ranked Flush" &&
    (Ranking(hand) ?= StraightFlush) :| "Ranked a StraightFlush"
  }

  property("StraightFlush has a greater score than any other hand Ranking") = forAll(genStraightFlush, genHand) {
    (strFlush, other) =>
      Ranking(other) != StraightFlush ==> {
        Ranking(strFlush) > Ranking(other)
      }
  }

  property("FourOfAKind must have 4 cards of same rank") = forAll(genFourOfAKind) { hand =>
    hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
      cards.length == 4
    }
  }

  property("a hand is 4 of a kind is FourOfAKind and NOT ThreeOfAKind") = forAll(genFourOfAKind) { hand =>
    (Ranking(hand) == FourOfAKind) ++ (Ranking(hand) != ThreeOfAKind)
  }

  property("a FullHouse has 4 or less ranks") = forAll(genFullHouse) { hand: Hand =>
    hand.cards.groupBy(_.rank).size <= 4
  }

  property("5 or more cards of a suit is a Flush") = forAll(genFlush, genNonFlush) { (flushHand, nonFlushHand) =>
    (Ranking(flushHand) != StraightFlush) :| "Constraint" ==> {
      (Ranking(flushHand) ?= Flush) :| "Flush" && (Ranking(nonFlushHand) != Flush) :| "Non-Flush"
    }
  }

  property("a Flush hand has 3 or less suits") = forAll(genFlush) { hand =>
    hand.cards.groupBy(c => c.suit).size <= 3
  }

  property("sequential cards rank a Straight") = forAll(genStraight) { hand =>
    all(Ranking(hand) != StraightFlush, Ranking(hand) != Flush) ==> {
      Ranking(hand) ?= Straight
    }
  }

  property("3 cards of the same rank is ThreeOfAKind") = forAll(genThreeOfAKind) { hand =>
    Ranking(hand) ?= ThreeOfAKind
  }

  property("at least 2 pair of cards in a hand is TwoPair") = forAll(genTwoPair) { hand =>
    all(Ranking(hand) != StraightFlush, Ranking(hand) != Flush, Ranking(hand) != Straight) ==> {
      Ranking(hand) ?= TwoPair
    }
  }

  property("at least two cards of the same rank is a Pair") = forAll(genPair) { hand =>
    Ranking(hand) ?= Pair
  }

  property("A HighCard hand has no other rank") = forAll(genHighCard) { hand =>
    (Ranking(hand) ?= HighCard)
  }

}
