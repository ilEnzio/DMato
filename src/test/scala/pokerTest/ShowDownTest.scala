package pokerTest

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import org.scalacheck.Prop.{all, forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import cats.implicits.{catsSyntaxOptionId, catsSyntaxPartialOrder}
import org.scalactic.anyvals.NonEmptySet
import poker.BoardState.River
import poker.OrderInstances._
import poker.Rank.rankMap
import poker._
import pokerData.HandGenerators._
import pokerData.SpecialHandsGenerators._

import scala.util.Random.shuffle

object ShowDownTest extends Properties("ShowDownTest") {

  property("StraightFlush beats FourOfKind, FullHouse") = forAll(genStraightFlush, genFourOfAKind, genFullHouse) {
    (strFlush, quads, boat) =>
      val testList = shuffle(List(quads, boat, strFlush))
      ShowDown(testList) ?= List(strFlush)
  }

  // TODO This is not a property
  property("A player with a Straight flush beats a player with Four of a Kind of the River") = {
    val pl1 = Player(Card(Ace, Spades), Card(Ace, Clubs))
    val pl2 = Player(Card(King, Hearts), Card(Queen, Hearts))
    val deck =
      Deck.makeStartingDeck.remove(List(Card(Ace, Spades), Card(Ace, Clubs), Card(King, Spades), Card(King, Hearts)))
    val river = River(
      List(pl1, pl2),
      deck,
      Card(Ace, Hearts),
      Card(Ace, Diamonds),
      Card(Jack, Hearts),
      Card(Ten, Hearts),
      Card(Nine, Hearts)
    )
    ShowDown.fromRiver(river) ?= Some(NonEmptySet(2))
  }

  property("For Two players the Showdown will award all winners") = {
    val startDeck  = Deck.makeStartingDeck.shuffle.unsafeRunSync()
    val boardCards = startDeck.take(9)
    val deck       = startDeck.drop(9)
    val pl1        = Player(boardCards(0), boardCards(1))
    val pl2        = Player(boardCards(2), boardCards(3))
    val river = River(
      List(pl1, pl2),
      deck,
      boardCards(4),
      boardCards(5),
      boardCards(6),
      boardCards(7),
      boardCards(8)
    )
    val (fst, snd) = (ShowDown.allHands(river)(0)._2, ShowDown.allHands(river)(1)._2)

    // if pl1 hand > pl2 => WinnerList(Map(1 -> 1, 2 -> 0))
    // pl2 > pl1 => WinnerList(Map(1 -> 0, 2 -> 1))
    // pl1 == pl2 => WinnerList(Map(1 -> .5, 2 -> .5))
    (fst, snd) match {
      case _ if fst > snd  => ShowDown.fromRiver(river) ?= Some(NonEmptySet(1))
      case _ if fst < snd  => ShowDown.fromRiver(river) ?= Some(NonEmptySet(2))
      case _ if fst == snd => ShowDown.fromRiver(river) ?= Some(NonEmptySet(1, 2))
    }
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
