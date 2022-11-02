package pokerTest

import org.scalacheck.Prop.{
  ?=,
  all,
  falsified,
  forAll,
  propBoolean,
  AnyOperators
}
import org.scalacheck.Properties
import poker.Hand._
import pokerData.HandGenerators._
import pokerData.SpecialHandsGenerators._
import poker._
import OrderInstances.handOrder
import cats.kernel.Comparison.GreaterThan
import org.scalacheck.Gen.const

object RankingTest extends Properties("Ranking Tests") {

//  property("A StraightFlush is not Ranked a Straight or Flush") = forAll(genStraightFlush) { hand =>
//    ( hand match {
//      case _: Straight      => false
//      case _: StraightFlush => true
//      case _                => false
//    }) :| "Not Ranked Straight" &&
//    (Hand.rank(hand.cards) match {
//      case _: Flush         => false
//      case _: StraightFlush => true
//      case _                => false
//    }) :| "Not Ranked Flush"
//
//  }

//  property("StraightFlush is greater than any other hand Ranking") = forAll(genStraightFlush, genHand) {
//    (strFlush, other) =>
//      (Hand.rank(other.cards) match {
//        case _: StraightFlush => false
//        case _                => true
//      }) ==> {
//        "StraightFlush beats Any other" |: handOrder.compare(strFlush, other) > 0
//      }
//  }

  property("StraightFlush: the Highest Ranked StraightFlush wins") =
    forAll(genNutStraightFlush, genNonNutStraightFlush) {
      (nutStrFlush, nonNutStrFlush) =>
        ("Nuts beats NonNuts" |: handOrder.compare(
          nutStrFlush,
          nonNutStrFlush
        ) > 0) &&
        ("NonNuts loses to Nuts" |: handOrder.compare(
          nonNutStrFlush,
          nutStrFlush
        ) < 0)
    }

//  property("FourOfAKind must have 4 cards of same rank") = forAll(genFourOfAKind) { hand =>
//    hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
//      cards.length == 4
//    }
//  }
//
//  property("a hand is 4 of a kind is FourOfAKind and NOT ThreeOfAKind") = forAll(genFourOfAKind) { hand =>
//    (Hand.rank(hand.cards) match {
//      case _: ThreeOfAKind => false
//      case _: FourOfAKind  => true
//      case _               => false
//    }) :| "Not ThreeOfAKind"
//  }

  property("Four of a Kind is greater than FullHouse") =
    forAll(genFourOfAKind, genFullHouse) { (quads, boat) =>
      handOrder.compare(quads, boat) > 0
    }

//  property("a FullHouse has 4 or less ranks") = forAll(genFullHouse) { hand =>
//    hand.cards.groupBy(_.rank).size <= 4
//  }
//
//  property("FullHouse: The highest set wins between two boats") = forAll(genFullHouse, genDeucesFullOfTres) {
//    (bigBoat, smBoat) =>
//      (bigBoat.cards.drop(2).map(_.rank) != smBoat.cards.map(_.rank)) ==> {
//        "BigBoat beats SmallBoat" |: handOrder.compare(bigBoat, smBoat) > 0
//      }
//  }

//  property("5 or more cards of a suit is a Flush") = forAll(genFlush, genNonFlush) { (flushHand, nonFlushHand) =>
//    (Hand.rank(flushHand.cards) match {
//      case _: StraightFlush => false
//      case _                => true
//    }) :| "Constraint" ==> {
//      (Hand.rank(flushHand.cards) match {
//        case _: Flush => true
//        case _        => false
//      }) :| "Flush" &&
//      (Hand.rank(nonFlushHand.cards) match {
//        case _: Flush => false
//        case _        => true
//      }) :| "Non-Flush"
//    }
//  }

//  property("a Flush hand has 3 or less suits") = forAll(genFlush) { hand =>
//    hand.cards.groupBy(c => c.suit).size <= 3
//  }

  property("Flush: NutFlush beats smaller flush") =
    forAll(genNutFlush, genNonNutFlush) { (nutFlush, nonNut) =>
      "Nut flush beats  NonNut flush" |: handOrder.compare(nutFlush, nonNut) > 0
    }

  property("Flush is greater than straight") = forAll(genFlush, genStraight) {
    (flush, straight) =>
      handOrder.compare(flush, straight) > 0
  }

//  property("sequential cards rank a Straight") = forAll(genStraight) { hand =>
//    all(
//      Hand.rank(hand.cards) match {
//        case _: StraightFlush => false
//        case _                => true
//      },
//      Hand.rank(hand.cards) match {
//        case _: Flush => false
//        case _        => true
//      }
//    ) ==> {
//      Hand.rank(hand.cards) match {
//        case _: Straight => true
//        case _           => false
//      }
//    }
//  }

//  property("Straight: The highest Ranked Straight wins") = forAll(genNutStraight, genNonNutStraight) {
//    (broadway, nonNutStraight) =>
//      (handOrder.compare(nonNutStraight, broadway) != 0) ==> {
//        (Hand.rank(nonNutStraight.cards) != Hand.rank(broadway.cards)) ==> {
//          handOrder.compare(broadway, nonNutStraight) > 0
//        }
//      }
//  }

  property("Straight: a non wheel beats a wheel straight") =
    forAll(genNonWheelStraight, genWheelStraight) { (straight, wheel) =>
      "NonWheel vs Wheel" |: handOrder.compare(straight, wheel) > 0
    }

  property("Straight is greater then Three of A Kind") =
    forAll(genStraight, genThreeOfAKind) { (straight, set) =>
      handOrder.compare(straight, set) > 0
    }

//  property("3 cards of the same rank is ThreeOfAKind") = forAll(genThreeOfAKind) { hand =>
//    Hand.rank(hand.cards) match {
//      case _: ThreeOfAKind => true
//      case _               => false
//    }
//  }
//
//  property("ThreeOfAKind has 5 ranks and is NOT a Straight") = forAll(genThreeOfAKind) { hand =>
//    (hand.cards.groupBy(_.rank).size ?= 5) &&
//    (Hand.rank(hand.cards) match {
//      case _: Straight => false
//      case _           => true
//    })
//  }

  property("Three of Kind is greater than TwoPair") =
    forAll(genThreeOfAKind, genTwoPair) { (set, twoPair) =>
      "Set vs TwoPair" |: handOrder.compare(set, twoPair) > 0
    }

  property("at least 2 pair of cards in a hand is TwoPair") =
    forAll(genTwoPair) { hand =>
//    (hand match {
//      case _: StraightFlush | _: Flush | _: Straight => false
//      case _                                         => true
//    }) ==> {
      hand match {
        case _: TwoPair => true
        case _          => false
      }
    }

//  property("Highest Ranked Two pair wins ") = forAll(genTwoPair) { hand =>
//    //    (hand match {
//    //      case _: StraightFlush | _: Flush | _: Straight => false
//    //      case _                                         => true
//    //    }) ==> {
//    hand match {
//      case _: TwoPair => true
//      case _          => false
//    }
//  }

// This seems dumb now.  I need to generator groups of cards
  // not the hand???
  property("at least two cards of the same rank is a Pair") = forAll(genPair) {
    hand =>
      hand match {
        case _: Pair => true
        case _       => false
      }
  }
// TODO I need another set of generators - hand card list genertors.
  property("A HighCard hand has no other rank") = forAll(genHighCard) { hand =>
    hand match {
      case _: HighCard => true
      case _           => false
    }
  }

}
