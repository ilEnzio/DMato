package Ranking

import deck.{Deck, Flush, FourOfAKind, Hand, HighCard, Rank, Ranking, ThreeOfAKind}
import deck.DeckPropTest.{genCard, genRank, genSuit}

import scala.util.Random
//import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

//object RankingTest extends Properties("RankingTests") {
class RankingTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
//  val handGen = genDeck.
// Seq.combinations function returns combinations of subsequences
  // so I should be able to use this to naively determine straights
  // order the cards this look for

  // a poker hand is 5 cards.  but I think for the purposes of this
  // it is the best 5 cards of 7, and the rank of the hand is determined by
  // the five cards
  val genHand = Hand(Deck.makeStartingDeck.shuffle.take(7))
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
  val genNonFlushHand = for {
    suit <- genSuit
  } yield {
    val (allCardsOfSuit1, remDeck) = (
      Deck.all.filter(_.suit == suit),
      Deck(Deck.all.filterNot(_.suit == suit))
    )
    val suitedShuffled = Random.shuffle(allCardsOfSuit1)
    Hand(suitedShuffled.take(4) ++ remDeck.shuffle.take(3))
  }

  implicit val arbHand = Arbitrary(genHand)

//  property("FourOfAKind must have 4 cards of same rank") = forAll { hand: Hand =>
//    // groupby
////    hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
////      cards.length == 4
//    if (Ranking(hand) == FourOfAKind)
//    }
//  }
  test("5 or more cards of a suit is a Flush") {
    forAll(genFlushHand, genNonFlushHand) { (flushHand, nonFlushHand) =>
//      val nonFlushHand   = Hand(suitedShuffled.take(4) ++ remDeck.shuffle.take(3))
      (Ranking(flushHand) == Flush) && Ranking(nonFlushHand) != Flush
      // || finalHand.cards.groupBy(c => c.suit).size >= 3
    }
  }

  test("a hand is 4 of a kind is FourOfAKind and NOT 3 of a kind") {
    forAll(genRank) { rank =>
      val (fourOfKind, remDeck) =
        (
          Deck.makeStartingDeck.cards.filter(_.rank == rank),
          Deck(Deck.makeStartingDeck.cards.filterNot(_.rank == rank))
        )
      val finalHand: Hand = Hand(fourOfKind ++ remDeck.shuffle.take(3))
      assert(Ranking(finalHand) == FourOfAKind && Ranking(finalHand) != ThreeOfAKind)
    }
  }

  test("3 of a kind is NOT 4 of a kind") {

    forAll(genRank) { rank1 =>
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
      Ranking(finalHand) != FourOfAKind && Ranking(finalHand) == ThreeOfAKind

    }
  }
  // later on the user will need to be able to choose cards
  // so I might have to implement that for this test...

}
