package Ranking

import deck.{Card, Deck, Flush, FourOfAKind, Hand, HighCard, Rank, Ranking, Straight, ThreeOfAKind}
import deck.DeckPropTest.{genCard, genRank, genSuit}
import org.scalacheck.Prop.{exists, forAll, propBoolean, AnyOperators}

import scala.util.Random
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Gen.pick
//import org.scalatest.funsuite.AnyFunSuite
//import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
//import org.scalacheck.Prop.forAll

object RankingTest extends Properties("RankingTest") {

  // a poker hand is 5 cards.  but I think for the purposes of this
  // it is the best 5 cards of 7, and the rank of the hand is determined by
  // the five cards
  implicit val arbHand = Arbitrary(genHand)
  implicit val arbRank = Arbitrary(genRank)
  val genHand          = Hand(Deck.makeStartingDeck.shuffle.take(7))

  val genFourOfKind = Gen.oneOf {
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
    val rank    = genRank.suchThat(_.value >= 5).sample.get
    val grouped = Deck.all.groupBy(_.rank).toList.sortBy(_._1.value)
    if (rank.value > 5) {
      val correctSlice: List[(Rank, List[Card])] =
        grouped.slice(rank.value - 6, rank.value - 1)
      Gen.oneOf(for {
        c1 <- correctSlice(0)._2
        c2 <- correctSlice(1)._2
        c3 <- correctSlice(2)._2
        c4 <- correctSlice(3)._2
        c5 <- correctSlice(4)._2
      } yield Hand(List(c1, c2, c3, c4, c5)))
    } else {
      val lowSlice: List[(Rank, List[Card])] =
        grouped.last :: grouped.slice(rank.value - 5, rank.value - 1)
      Gen.oneOf(for {
        c1 <- lowSlice(0)._2
        c2 <- lowSlice(1)._2
        c3 <- lowSlice(2)._2
        c4 <- lowSlice(3)._2
        c5 <- lowSlice(4)._2
      } yield Hand(List(c1, c2, c3, c4, c5)))
    }
  }

  property("FourOfAKind must have 4 cards of same rank") = forAll(genFourOfKind) { hand =>
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

  //ToDo: This test some times fails
  property("3 of a kind is NOT 4 of a kind") = forAll(genRank) { rank1 =>
    val (fourCards, remDeck) =
      (
        Deck.all.filter(_.rank == rank1),
        Deck(Deck.all.filterNot(_.rank == rank1))
      )
    val threeOfAKind = fourCards.tail
    val rank2        = Gen.oneOf(Rank.all.filter(_ != rank1)).sample.get
    val (altFour, remDeck2) =
      (
        remDeck.cards.filter(_.rank == rank2),
        Deck(remDeck.cards.filterNot(_.rank == rank1))
      )
    val finalHand: Hand = Hand(altFour.head :: threeOfAKind ++ remDeck2.shuffle.take(3))
    (Ranking(finalHand) =? FourOfAKind) ++ (Ranking(finalHand) == ThreeOfAKind)
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
// Todo: This test some times fail due to seed issue
  property("value difference between high and low end of Straight is 4") = forAll(genStraightHand) { hand: Hand =>
    // sort the cards

    val sortedCards = hand.cards
      .sortBy(_.rank.value)
      .reverse
    // Create an Ordering So that I don't have to reach into value

    (Ranking(hand) ?= Straight) && (sortedCards(0).rank.value - sortedCards(4).rank.value == 4 ||
      sortedCards(0).rank.value - sortedCards(4).rank.value == 12)
  }

  property("sequential cards rank a Straight") = forAll(genStraightHand) { hand =>
//    println(s"HandRank: ${Ranking(hand)}")
    (Ranking(hand) != Flush) ==> {
      Ranking(hand) ?= Straight
    }
  }

  // TODO test for straight flush

}
