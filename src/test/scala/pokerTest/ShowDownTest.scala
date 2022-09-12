package pokerTest

import org.scalacheck.Prop.{all, forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import cats.implicits.{catsSyntaxPartialOrder, toFoldableOps}
import poker.OrderInstances._
import poker.Rank.rankMap
import poker._
import pokerData.HandGenerators._
import pokerData.SpecialHandsGenerators._

import scala.util.Random.shuffle

object ShowDownTest extends Properties("ShowDownTest") {
// TODO this is not really a showdown test

  property("StraightFlush beats FourOfKind, FullHouse") = forAll(genStraightFlush, genFourOfAKind, genFullHouse) {
    (strFlush, quads, boat) =>
      val testList = shuffle(List(quads, boat, strFlush))
      ShowDown(testList) ?= List(strFlush)
  }
// TODO Either I have to change the api/model or change the tests... I think the model
  property("FourOfAKind beats FullHouse, Flush") = forAll(genFourOfAKind, genFullHouse, genNutFlush) {
    (quads, boat, flush) =>
      val testList = shuffle(List(boat, flush, quads))
      (ShowDown(testList) ?= List(quads)) &&
      (ShowDown(testList) != List(flush))
  }

  property("FullHouse beats Flush, Straight, ThreeOfKind, TwoPair") =
    forAll(genFullHouse, genNonNutFlush, genStraight, genThreeOfAKind, genTwoPair) {
      (boat, flush, straight, set, twoPair) =>
        val testList = shuffle(List(set, twoPair, flush, boat, straight))
        ShowDown(testList) ?= List(boat)
    }

  property("Flush beats a straight, ThreeOfAKind") = forAll(genFlush, genStraight, genThreeOfAKind) {
    (flush, straight, set) =>
      val testList = shuffle(List(straight, set, flush))
      ShowDown(testList) ?= List(flush)
  }

  property("Straight: Two Equally Ranked Straights win vs Lower hands") =
    forAll(genStraight, genThreeOfAKind, genTwoPair) { (straight, set, twoPair) =>
      val straight2 = straight.copy()
      val testList  = List(straight, set, twoPair, straight2)
      (ShowDown(testList).size == 2) &&
      ShowDown(testList).forall(List(straight2, straight).contains(_))
    }

  property("Straight beats ThreeOfAKind, TwoPair, Pair, HighCard") =
    forAll(genStraight, genThreeOfAKind, genTwoPair, genPair, genHighCard) { (straight, set, twoPair, pair, highCard) =>
      val testList = shuffle(List(set, twoPair, pair, highCard, straight))
      (ShowDown(testList) ?= List(straight)) &&
      ShowDown(testList) != List(highCard)
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
      (Hand.rank(aHigh.cards) > Hand.rank(highCard.cards)) ==> {
        val aHigh2   = aHigh.copy()
        val testList = List(aHigh, aHigh2, highCard)
        List(aHigh, aHigh2).forall(ShowDown(testList).contains(_)) &&
        (ShowDown(testList).size ?= 2) &&
        (Hand.rank(aHigh.cards) ?= Hand.rank(aHigh2.cards))
      }
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

    val winningHand = Hand.rank(oCardList.take(5))
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
      val winningHand = Hand.rank(oCardList.take(5))
      val dupeAHigh   = createDupe(4)
      val testList    = shuffle(List(winningHand, other, dupeAHigh))
      (ShowDown(testList)) ?= List(winningHand)
    }
  }
}
