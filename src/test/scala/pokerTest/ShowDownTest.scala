package pokerTest

import org.scalacheck.Prop.{all, forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import cats.implicits.catsSyntaxPartialOrder
import poker.OrderInstances._
import poker.Rank.rankMap
import poker._
import test.pokerData.DataGenerators._

import scala.util.Random.shuffle

// TODO Account for all Ties - for instance players can be playing the boards

object ShowDownTest extends Properties("ShowDownTest") {

  property("StraightFlush is greater than any other hand Ranking") = forAll(genStraightFlush, genHand) {
    (strFlush, other) =>
      HandRank(other) != StraightFlush ==> {
        HandRank(strFlush) > HandRank(other)
      }
  }

  property("StraightFlush: the Highest Ranked StraightFlush wins") =
    forAll(genNutStraightFlush, genNonNutStraightFlush) { (nutStrFlush, nonNutStrFlush) =>
      val testList = shuffle(List(nonNutStrFlush, nutStrFlush))

      "Nuts vs NonNuts" |: (ShowDown(testList) ?= List(nutStrFlush, nonNutStrFlush))
      (ShowDown(testList) != List(nonNutStrFlush))
    }

  property("StraightFlush beats FourOfKind, FullHouse") = forAll(genStraightFlush, genFourOfAKind, genFullHouse) {
    (strFlush, quads, boat) =>
      val testList = shuffle(List(quads, boat, strFlush))
      ShowDown(testList) ?= List(strFlush)
  }

  property("FourOfAKind beats FullHouse, Flush") = forAll(genFourOfAKind, genFullHouse, genNutFlush) {
    (quads, boat, flush) =>
      val testList = shuffle(List(boat, flush, quads))
      (ShowDown(testList) ?= List(quads)) &&
      (ShowDown(testList) != List(flush))
  }

  property("Four of a Kind is greater than FullHouse") = forAll(genFourOfAKind, genFullHouse) { (quads, boat) =>
    HandRank(quads) > HandRank(boat)
  }

  property("FullHouse: The highest set wins between two boats") = forAll(genFullHouse, genDeucesFullOfTres) {
    (bigBoat, smBoat) =>
      (bigBoat.cards.drop(2).map(_.rank) != smBoat.cards.map(_.rank)) ==> {
        val testList = shuffle(List(smBoat, bigBoat))
        "BigBoat vs SmallBoat" |: (ShowDown(testList) ?= List(bigBoat))
      }
  }

  property("FullHouse beats Flush, Straight, ThreeOfKind, TwoPair") = // ToDO Include Flush
    forAll(genFullHouse, genNonNutFlush, genStraight, genThreeOfAKind, genTwoPair) {
      (boat, flush, straight, set, twoPair) =>
        val testList = shuffle(List(set, twoPair, flush, boat, straight))
        ShowDown(testList) ?= List(boat)
    }

  property("Flush: NutFlush beats smaller flush") = forAll(genNutFlush, genNonNutFlush) { (nutFlush, nonNut) =>
    val testList = shuffle(List(nonNut, nutFlush))
    "Nut vs NonNut" |: (ShowDown(testList) ?= List(nutFlush))
  }

  property("Flush beats a straight, ThreeOfAKind") = forAll(genFlush, genStraight, genThreeOfAKind) {
    (flush, straight, set) =>
      val testList = shuffle(List(straight, set, flush))
      (ShowDown(testList) ?= List(flush))
  }

  property("Flush is greater than straight") = forAll(genFlush, genStraight) { (flush, straight) =>
    HandRank(flush) > HandRank(straight)
  }

  property("Straight: Two Equally Ranked Straights win vs Lower hands") =
    forAll(genStraight, genThreeOfAKind, genTwoPair) { (straight, set, twoPair) =>
      val straight2 = straight.copy()
      val testList  = List(straight, set, twoPair, straight2)
      (ShowDown(testList).size == 2) &&
      (ShowDown(testList).forall(List(straight2, straight).contains(_)))
    }

  property("Straight: The highest Ranked Straight wins") = forAll(genNutStraight, genStraight) {
    (broadway, nonNutStraight) =>
      (straightOrder.compare(nonNutStraight, broadway) != 0) ==> {

        val testList = shuffle(List(broadway, nonNutStraight))
        (ShowDown(testList)) ?= List(broadway)
      }
  }
  property("Straight: a non wheel beats a wheel straight") = forAll(genNonWheelStraight, genWheelStraight) {
    (straight, wheel) =>
      val testList = shuffle(List(wheel, straight))
      "NonWheel vs Wheel" |: (ShowDown(testList) ?= List(straight))
  }

  property("Straight beats ThreeOfAKind, TwoPair, Pair, HighCard") =
    forAll(genStraight, genThreeOfAKind, genTwoPair, genPair, genHighCard) { (straight, set, twoPair, pair, highCard) =>
      val testList = shuffle(List(set, twoPair, pair, highCard, straight))
      (ShowDown(testList) ?= List(straight)) &&
      ShowDown(testList) != List(highCard)
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
      ShowDown(testList) ?= List(set)
    }

  property("Two Pair beats Pair and HighCard") = forAll(genTwoPair, genPair, genHighCard) { (twoPair, pair, highCard) =>
    val testList = shuffle(List(highCard, twoPair, pair))
    all(
      "TwoPair, Pair, HighCard" |: (ShowDown(testList) ?= List(twoPair)),
      "Not Equal" |: (ShowDown(testList) != List(highCard))
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

    "Pair, Ace High, HighCard" |: (ShowDown(testList) ?= List(pair))
  }

  property("HighCard: two high card hands of the same value are equal") = forAll(genAceHigh, genHighCard) {
    (aHigh, highCard) =>
      val aHigh2   = aHigh.copy()
      val testList = List(aHigh, aHigh2, highCard)
      List(aHigh, aHigh2).forall(ShowDown(testList).contains(_)) &&
      (ShowDown(testList).size ?= 2) &&
      (HandRank(aHigh) ?= HandRank(aHigh2))
  }

  property("HighCard - Ace high is greater than any other high card") = forAll(genAceHigh, genHighCard) {
    (aHigh, other) =>
      (other.cards.sorted.reverse(0).rank != Ace) ==> {
        "Ace vs Other" |: (ShowDown(List(aHigh, other)) ?= List(aHigh))
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

    (ShowDown(List(winningHand, dupe1)) ?= List(winningHand)) &&
    (ShowDown(List(dupe2, winningHand)) ?= List(winningHand)) &&
    (ShowDown(List(dupe3, winningHand)) ?= List(winningHand)) &&
    (ShowDown(List(dupe4, winningHand)) ?= List(winningHand)) && ("Not Equal" |: (ShowDown(
      List(dupe1, winningHand)
    ) != List(dupe1)))
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
      (ShowDown(testList)) ?= List(winningHand)
    }
  }
}
