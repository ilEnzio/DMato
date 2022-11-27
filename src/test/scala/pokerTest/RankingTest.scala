package pokerTest

import org.scalacheck.Prop.{all, forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import poker.Hand._
import pokerData.HandGenerators._
import pokerData.SpecialHandsGenerators._
import poker._
import OrderInstances._
import cats.implicits.{catsSyntaxEq, catsSyntaxPartialOrder}
import cats.kernel.Comparison
import cats.kernel.Comparison._
import pokerData.DeckGenerators._

object RankingTest extends Properties("Ranking Tests") {

  property("A StraightFlush is not Ranked a Straight or Flush") =
    forAll(genStraightFlushCards) { cards =>
      (Hand.rank(cards) match {
        case _: Straight      => false
        case _: StraightFlush => true
        case _                => false
      }) :| "Not Ranked Straight" &&
      (Hand.rank(cards) match {
        case _: Flush         => false
        case _: StraightFlush => true
        case _                => false
      }) :| "Not Ranked Flush"

    }

  property("StraightFlush is greater than any other hand Ranking") =
    forAll(genStraightFlushCards, genHandCards) { (strFlush, other) =>
      (Hand.rank(other) match {
        case _: StraightFlush => false
        case _                => true
      }) ==> {
        "StraightFlush beats Any other" |: handOrder.compare(
          Hand.rank(strFlush),
          Hand.rank(other)
        ) > 0
      }
    }

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

  property("FourOfAKind must have 4 cards of same rank") =
    forAll(genFourOfAKindCards) { cards =>
      cards.groupBy(c => c.rank).exists { case (_, cards) =>
        cards.length == 4
      }
    }

  property("a hand is 4 of a kind is FourOfAKind and NOT ThreeOfAKind") =
    forAll(genFourOfAKindCards) { cards =>
      (Hand.rank(cards) match {
        case _: ThreeOfAKind => false
        case _: FourOfAKind  => true
        case _               => false
      }) :| "Not ThreeOfAKind"
    }

  property("Four of a Kind is greater than FullHouse") =
    forAll(genFourOfAKind, genFullHouse) { (quads, boat) =>
      handOrder.compare(quads, boat) > 0
    }

  property("a FullHouse has 4 or less ranks") = forAll(genFullHouseCards) {
    cards =>
      cards.groupBy(_.rank).size <= 4
  }

  // TODO these tests are still too complicated... Will elminate them.
  property(
    "FullHouse: The winner between FullHouse is determined by the Set ranking first."
  ) = forAll { (rank1: Rank, rank2: Rank) =>
    val boat1 = FullHouse(rank1, Two)
    val boat2 = FullHouse(rank2, Two)

    rankOrder.comparison(rank1, rank2) match {
      case GreaterThan => handOrder.compare(boat1, boat2) > 0
      case LessThan    => handOrder.compare(boat1, boat2) < 0
      case EqualTo     => handOrder.compare(boat1, boat2) == 0
    }
  }

  property(
    "FullHouse: The winner between FullHouses of same Set rank is determined by Pair ranking."
  ) = forAll { (rank1: Rank, rank2: Rank) =>
    val boat1 = FullHouse(Ace, rank1)
    val boat2 = FullHouse(Ace, rank2)

    rankOrder.comparison(rank1, rank2) match {
      case GreaterThan => handOrder.compare(boat1, boat2) > 0
      case LessThan    => handOrder.compare(boat1, boat2) < 0
      case EqualTo     => handOrder.compare(boat1, boat2) == 0
    }

  }

  property("FullHouse is properly Ranked") = forAll(genFullHouseCards)(cards =>
    Hand.rank(cards) match {
      case x: FullHouse =>
        cards.count(_.rank === x.rank1) > cards.count(
          _.rank === x.rank2
        ) || x.rank1 > x.rank2
      case _ => false
    }
  )

  property("5 or more cards of a suit is a Flush") =
    forAll(genFlushCards, genFourFlushCards) { (flushCards, fourFlushCards) =>
      (flushCards
        .groupBy(_.suit)
        .toList
        .map(_._2.size)
        .max >= 5) &&
      (Hand.rank(flushCards) match {
        case _: Flush => true
        case _        => false
      }) &&
      (fourFlushCards
        .groupBy(_.suit)
        .toList
        .map(_._2.size)
        .max < 5) &&
      (Hand.rank(fourFlushCards) match {
        case _: StraightFlush => false
        case _: Flush         => false
        case _                => true
      })

    }

  property("a Flush hand has 3 or less suits") = forAll(genFlushCards) {
    cards =>
      cards.groupBy(c => c.suit).size <= 3
  }

  property("Flush: NutFlush beats smaller flush") =
    forAll(genNutFlush, genNonNutFlush) { (nutFlush, nonNut) =>
      "Nut flush beats  NonNut flush" |: handOrder.compare(nutFlush, nonNut) > 0
    }

  property("Flush is greater than straight") = forAll(genFlush, genStraight) {
    (flush, straight) =>
      handOrder.compare(flush, straight) > 0
  }

  property("sequential cards rank a Straight") = forAll(genStraightCards) {
    cards =>
      all(
        Hand.rank(cards) match {
          case _: StraightFlush => false
          case _                => true
        },
        Hand.rank(cards) match {
          case _: Flush => false
          case _        => true
        }
      ) ==> {
        Hand.rank(cards) match {
          case _: Straight => true
          case _           => false
        }
      }
  }

  property("Straight: The highest Ranked Straight wins") =
    forAll(genNutStraightCards, genNonNutStraightCards) {
      (broadway, nonNutStraight) =>
        (handOrder.compare(
          Hand.rank(nonNutStraight),
          Hand.rank(broadway)
        ) != 0) ==> {
          (Hand.rank(nonNutStraight) != Hand.rank(broadway)) ==> {
            handOrder.compare(
              Hand.rank(broadway),
              Hand.rank(nonNutStraight)
            ) > 0
          }
        }
    }

  property("Straight: a non wheel beats a wheel straight") =
    forAll(genNonWheelStraight, genWheelStraight) { (straight, wheel) =>
      "NonWheel vs Wheel" |: handOrder.compare(straight, wheel) > 0
    }

  property("Straight is greater then Three of A Kind") =
    forAll(genStraight, genThreeOfAKind) { (straight, set) =>
      handOrder.compare(straight, set) > 0
    }

  property("3 cards of the same rank is ThreeOfAKind") =
    forAll(genThreeOfAKindCards) { cards =>
      (cards
        .groupBy(_.rank)
        .toList
        .map(_._2.size)
        .max === 3) &&
      (Hand.rank(cards) match {
        case _: ThreeOfAKind => true
        case _               => false
      })
    }

  property("ThreeOfAKind has 5 ranks and is NOT a Straight") =
    forAll(genThreeOfAKindCards) { cards =>
      (cards.groupBy(_.rank).size ?= 5) &&
      (Hand.rank(cards) match {
        case _: Straight => false
        case _           => true
      })
    }

  property("Three of Kind is greater than TwoPair") =
    forAll(genThreeOfAKind, genTwoPair) { (set, twoPair) =>
      "Set vs TwoPair" |: handOrder.compare(set, twoPair) > 0
    }

  property("at least 2 pair of cards in a hand is TwoPair") =
    forAll(genTwoPairCards) { cards =>
      (cards
        .groupBy(_.rank)
        .toList
        .map(_._2)
        .count(_.length == 2) >= 2) &&
      (Hand.rank(cards) match {
        case _: TwoPair => true
        case _          => false
      })
    }

  property("Highest Ranked Two pair wins ") = forAll {
    (rank1: Rank, rank2: Rank, card1: Card, card2: Card) =>
      val kickers = List(card1, card2)
      val twoP1   = TwoPair(rank1, Two, kickers)
      val twoP2   = TwoPair(rank2, Two, kickers)

      rankOrder.comparison(rank1, rank2) match {
        case GreaterThan => handOrder.compare(twoP1, twoP2) > 0
        case EqualTo     => handOrder.compare(twoP1, twoP2) == 0
        case LessThan    => handOrder.compare(twoP1, twoP2) < 0
      }
  }

  property("at least two cards of the same rank is a Pair") =
    forAll(genPairCards) { cards =>
      (cards
        .groupBy(_.rank)
        .toList
        .map(_._2)
        .count(_.length == 2) == 1) &&
      (Hand.rank(cards) match {
        case _: Pair => true
        case _       => false
      })
    }

  property("A HighCard hand has no other rank") = forAll(genHighCardCards) {
    // TODO Actually it might be better to get rid of these wild card
    // pattern matches and use the Set (A => Boolean) pattern
    cards =>
      Hand.rank(cards) match {
        case _: HighCard => true
        case _           => false
      }
  }

}
