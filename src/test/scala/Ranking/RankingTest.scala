package Ranking

import deck.{Card, Deck, Flush, FourOfAKind, Hand, HighCard, Rank, Ranking, Straight, ThreeOfAKind}
import deck.DeckPropTest.{genCard, genRank, genSuit}
import org.scalacheck.Prop.{all, exists, forAll, propBoolean, AnyOperators}

import scala.util.Random
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Gen.{oneOf, pick}
//import org.scalatest.funsuite.AnyFunSuite
//import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
//import org.scalacheck.Prop.forAll

object RankingTest extends Properties("RankingTest") {

  // a poker hand is 5 cards.  but I think for the purposes of this
  // it is the best 5 cards of 7, and the rank of the hand is determined by
  // the five cards
  implicit val arbHand = Arbitrary(genHand)
  implicit val arbRank = Arbitrary(genRank)
  val genHand          = Gen.pick(7, Deck.all)

  val genThreeOfAKind = Gen.oneOf {
    for {
      group <- Deck.all.groupBy(_.rank).toList.map(_._2)
      rank = group(0).rank
      card1 <- genCard.suchThat(c => c.rank != rank).sample
      card2 <- genCard.suchThat(c => c != card1 && c.rank != rank).sample
      card3 <- genCard.suchThat(c => c != card1 && c != card2 && c.rank != rank).sample
      card4 <- genCard.suchThat(c => c != card1 && c != card2 && c != card3 && c.rank != rank).sample
    } yield Hand(card1 :: card2 :: card3 :: card4 :: group.take(3))
  }

  val genFourOfAKind = Gen.oneOf {
    for {
      group <- Deck.all.groupBy(_.rank).toList.map(_._2)
      rank = group(0).rank
      card1 <- genCard.suchThat(c => c.rank != rank).sample
      card2 <- genCard.suchThat(c => c != card1 && c.rank != rank).sample
      card3 <- genCard.suchThat(c => c != card1 && c != card2 && c.rank != rank).sample
    } yield Hand(card1 :: card2 :: card3 :: group)
  }

  val genFlushHand = for {
    suit <- genSuit
  } yield {
    val (allCardsOfSuit1, remDeck) = (
      Deck.all.filter(_.suit == suit),
      Deck(Deck.all.filterNot(_.suit == suit))
    )
    val suitedShuffled = Random.shuffle(allCardsOfSuit1)
    Hand(suitedShuffled.take(5) ++ remDeck.add(suitedShuffled.drop(5)).shuffle.take(2))
  }
  val genNonFlushHand: Gen[Hand] = for {
    suit <- genSuit
  } yield {
    val (allCardsOfSuit1, remDeck) = (
      Deck.all.filter(_.suit == suit),
      Deck(Deck.all.filterNot(_.suit == suit))
    )
    val suitedShuffled = Random.shuffle(allCardsOfSuit1)
    Hand(suitedShuffled.take(4) ++ remDeck.shuffle.take(3))
  }

  /// group card by rank; pick a rank between A-5
  // take a card from that top rank and 4 ranks beneath it.
  // 5 is a special case
  // 0-2, 1-3,2-4,3-5,4-6,5-7,6-8,7-9,8-10,9-11,10-12,11-12,12-14

  val genStraightHand: Gen[Hand] = {
    val rank    = genRank.retryUntil(_.value >= 5).sample.get
    val grouped = Deck.all.groupBy(_.rank).toList.sortBy(_._1.value)
    if (rank.value > 5) {
      val correctSlice: List[(Rank, List[Card])] =
        grouped.slice(rank.value - 6, rank.value - 1)
      "Hi-Hand" |: Gen.oneOf(for {
        c1 <- correctSlice(0)._2
        c2 <- correctSlice(1)._2
        c3 <- correctSlice(2)._2
        c4 <- correctSlice(3)._2
        c5 <- correctSlice(4)._2
        n1 <- genCard.suchThat(c => List(c1, c2, c3, c4, c5).contains(c) == false).sample
        n2 <- genCard.suchThat(c => List(c1, c2, c3, c4, c5, n1).contains(c) == false).sample
      } yield Hand(List(c1, c2, c3, c4, c5, n1, n2)))
    } else {
      val lowSlice: List[(Rank, List[Card])] = {
        println(s"Last: ${grouped.last}")
        grouped.last :: grouped.slice(rank.value - 5, rank.value - 1)
      }
      "Lo-Hand" |: Gen.oneOf(for {
        c1 <- lowSlice(0)._2
        c2 <- lowSlice(1)._2
        c3 <- lowSlice(2)._2
        c4 <- lowSlice(3)._2
        c5 <- lowSlice(4)._2
        n1 <- genCard.suchThat(c => List(c1, c2, c3, c4, c5).contains(c) == false).sample
        n2 <- genCard.suchThat(c => List(c1, c2, c3, c4, c5, n1).contains(c) == false).sample
      } yield Hand(List(c1, c2, c3, c4, c5, n1, n2)))
    }
  }

  property("FourOfAKind must have 4 cards of same rank") = forAll(genFourOfAKind) { hand =>
    (Ranking(hand) == FourOfAKind) ==> {
      hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
        cards.length == 4
      }
    }
  }

  property("a hand is 4 of a kind is FourOfAKind and NOT 3 of a kind") = forAll { rank: Rank =>
    val (fourOfKind, remDeck) =
      (
        Deck.makeStartingDeck.cards.filter(_.rank == rank),
        Deck(Deck.makeStartingDeck.cards.filterNot(_.rank == rank))
      )
    val finalHand: Hand = Hand(fourOfKind ++ remDeck.shuffle.take(3))
    (Ranking(finalHand) == FourOfAKind) ++ (Ranking(finalHand) != ThreeOfAKind)
  }

  property("3 of a kind is NOT 4 of a kind") = forAll(genThreeOfAKind) { hand =>
    (Ranking(hand) != FourOfAKind) ++ (Ranking(hand) == ThreeOfAKind)
  }
  // later on the user will need to be able to choose cards
  // so I might have to implement that for this test...

  property("5 or more cards of a suit is a Flush") = forAll(genFlushHand, genNonFlushHand) {
    (flushHand, nonFlushHand) =>
      (Ranking(flushHand) == Flush) && Ranking(nonFlushHand) != Flush
  }

  property("a Flush hand has 3 or less suits") = forAll(genFlushHand) { hand =>
    (Ranking(hand) == Flush) ==> {
      hand.cards.groupBy(c => c.suit).size <= 3
    }
  }

  property("sequential cards rank a Straight") = forAll(genStraightHand) { hand =>
    (Ranking(hand) != Flush) ==> {
      Ranking(hand) ?= Straight
    }
  }

  // TODO test for straight flush

}
