package pokerTest

import org.scalacheck.Prop.{all, forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import cats.implicits.catsSyntaxPartialOrder
import poker.OrderInstances._
import poker.Rank.rankMap
import poker._
import test.pokerData.DataGenerators._

import scala.util.Random.shuffle

object ShowDownTest extends Properties("ShowDownTest") {

  property("StraightFlush is greater than any other hand Ranking") = forAll(genStraightFlush, genHand) {
    (strFlush, other) =>
      HandRank(other) != StraightFlush ==> {
        HandRank(strFlush) > HandRank(other)
      }
  }

  property("FullHouse: The highest set wins between two boats") = forAll(genFullHouse, genDeucesFullOfTres) {
    (bigBoat, smBoat) =>
      (bigBoat.cards.drop(2).map(_.rank) != smBoat.cards.map(_.rank)) ==> {
        val testList = shuffle(List(smBoat, bigBoat))
        "BigBoat vs SmallBoat" |: (ShowDown(testList) ?= List(bigBoat, smBoat))
      }
  }

  property("FullHouse beats Straight, ThreeOfKind, TwoPair") =
    forAll(genFullHouse, genStraight, genThreeOfAKind, genTwoPair) { (boat, straight, set, twoPair) =>
      val testList = shuffle(List(set, twoPair, boat, straight))
      ShowDown(testList) ?= List(boat, straight, set, twoPair)
    }

  property("Straight: a non wheel beats a wheel straight") = forAll(genNonWheelStraight, genWheelStraight) {
    (straight, wheel) =>
      val testList = shuffle(List(wheel, straight))
      "NonWheel vs Wheel" |: (ShowDown(testList) ?= List(straight, wheel))
  }

  property("Straight beats ThreeOfAKind, TwoPair, Pair, HighCard") =
    forAll(genStraight, genThreeOfAKind, genTwoPair, genPair, genHighCard) { (straight, set, twoPair, pair, highCard) =>
      val testList = shuffle(List(set, twoPair, pair, highCard, straight))
      (ShowDown(testList) ?= List(straight, set, twoPair, pair, highCard)) &&
      ShowDown(testList) != List(highCard, set, twoPair, pair, straight)
    }

  property("Straight is greater then Three of A Kind") = forAll(genStraight, genThreeOfAKind) { (straight, set) =>
    HandRank(straight) > HandRank(set)
  }

  property("Three of Kind is greater than TwoPair") = forAll(genThreeOfAKind, genTwoPair) { (set, twoPair) =>
    "Set vs TwoPair" |: (HandRank(set) > HandRank(twoPair))
  }

  property("Three of a Kind beats TwoPair, Pair, HighCard") =
    forAll(genThreeOfAKind, genTwoPair, genPair, genHighCard) { (set, twoPair, pair, highCard) =>
      val testList = shuffle(List(pair, highCard, set, twoPair))
      ShowDown(testList) ?= List(set, twoPair, pair, highCard)
    }

  property("Two Pair beats Pair and HighCard") = forAll(genTwoPair, genPair, genHighCard) { (twoPair, pair, highCard) =>
    val testList = shuffle(List(highCard, twoPair, pair))
    all(
      "TwoPair, Pair, HighCard" |: (ShowDown(testList) ?= List(twoPair, pair, highCard)),
      "Not Equal" |: (ShowDown(testList) != List(highCard, pair, twoPair))
    )
  }

  property("A Pair beats HighCard at show down ") = forAll(genPair, genAceHigh) { (pair, aHigh) =>
    val oCardList = aHigh.cards.sorted.reverse

    def createDupe(idx: Int): Hand = {
      val cardIdxRank = oCardList(4).rank
      val newCard =
        if (cardIdxRank.value == 2) oCardList(idx)
        else oCardList(idx).copy(rank = rankMap(cardIdxRank.value - 1))
      aHigh.copy(cards = oCardList.take(4) ++ List(newCard))
    }

    val dupe1    = createDupe(4)
    val testList = shuffle(List(dupe1, pair, aHigh))
    "Pair, Ace High, HighCard" |: (ShowDown(testList) ?= List(pair, aHigh, dupe1))
  }

  property("HighCard - Ace high is greater than any other high card") = forAll(genAceHigh, genHighCard) {
    (aHigh, other) =>
      (other.cards.sorted.reverse(0).rank != Ace) ==> {
        "Ace vs Other" |: (ShowDown(List(aHigh, other)) ?= List(aHigh, other))
      }
  }

  property("HighCard - General test of all Corresponding cards") = forAll(genAceHigh) { original =>
    val oCardList = original.cards.sorted.reverse

    def createDupe(idx: Int): Hand = {
      val cardIdxRank = oCardList(4).rank
      val newCard =
        if (cardIdxRank.value == 2) oCardList(idx)
        else oCardList(idx).copy(rank = rankMap(cardIdxRank.value - 1))
      original.copy(cards = oCardList.take(4) ++ List(newCard))
    }

    val winningHand = Hand(oCardList.take(5))
    val dupe1       = createDupe(4)
    val dupe2       = createDupe(3)
    val dupe3       = createDupe(2)
    val dupe4       = createDupe(1)

    (ShowDown(List(winningHand, dupe1)) ?= List(winningHand, dupe1)) &&
    (ShowDown(List(dupe2, winningHand)) ?= List(winningHand, dupe2)) &&
    (ShowDown(List(dupe3, winningHand)) ?= List(winningHand, dupe3)) &&
    (ShowDown(List(dupe4, winningHand)) =? List(winningHand, dupe4)) && ("Not Equal" |: (ShowDown(
      List(dupe1, winningHand)
    ) != List(dupe1, winningHand)))
  }

  property("HighCard - Multiple AHigh, and other HighCArd") = forAll(genAceHigh, genHighCard) { (aHigh, other) =>
    (other.cards.sorted.reverse(0).rank != Ace) ==> {

      val oCardList = aHigh.cards.sorted.reverse
      def createDupe(idx: Int): Hand = {
        val cardIdxRank = oCardList(4).rank
        val newCard =
          if (cardIdxRank.value == 2) oCardList(idx)
          else oCardList(idx).copy(rank = rankMap(cardIdxRank.value - 1))
        aHigh.copy(cards = oCardList.take(4) ++ List(newCard))
      }
      val winningHand = Hand(oCardList.take(5))
      val dupeAHigh   = createDupe(4)
      val testList    = shuffle(List(winningHand, other, dupeAHigh))
      (ShowDown(testList)) ?= List(winningHand, dupeAHigh, other)
    }
  }
}