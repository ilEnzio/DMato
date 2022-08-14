package Ranking

import deck.{Card, Deck, Flush, FourOfAKind, FullHouse, Hand, HighCard, Rank, Ranking, Straight, ThreeOfAKind}
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
  val genHand          = Gen.pick(7, Deck.all)
  implicit val arbHand = Arbitrary(genHand)
  implicit val arbRank = Arbitrary(genRank)

  // Optimal Output Generators

  val genFourOfAKind: Gen[Hand] =
    for {
      rank <- genRank
      quads = Deck.all.groupBy(_.rank)(rank)
      card1 <- genCard.suchThat(c => c.rank != rank)
      card2 <- genCard.suchThat(c => c != card1 && c.rank != rank)
      card3 <- genCard.suchThat(c => c != card1 && c != card2 && c.rank != rank)
    } yield Hand(card1 :: card2 :: card3 :: quads)

  val genThreeOfAKind: Gen[Hand] =
    for {
      rank <- genRank
      grouped = Deck.all.groupBy(_.rank)
      set   <- pick(3, grouped(rank))
      card1 <- genCard.suchThat(c => c.rank != rank)
      card2 <- genCard.suchThat(c => c != card1 && c.rank != rank)
      card3 <- genCard.suchThat(c => c != card1 && c != card2 && c.rank != rank)
      card4 <- genCard.suchThat(c => c != card1 && c != card2 && c != card3 && c.rank != rank)
    } yield Hand(card1 :: card2 :: card3 :: card4 :: set.toList)

  val genFullHouse: Gen[Hand] =
    for {
      rank1 <- genRank
      rank2 <- genRank.suchThat(r => r != rank1)
      grouped = Deck.all.groupBy(_.rank)
      set  <- pick(3, grouped(rank1))
      pair <- pick(2, grouped(rank2))
      card1 <- genCard.suchThat { c =>
        !set.contains(c) &&
        !pair.contains(c) &&
        c.rank != rank1 &&
        c.rank != rank2
      }
      card2 <- genCard.suchThat(c =>
        !set.contains(c) &&
          !pair.contains(c) &&
          c.rank != rank1 &&
          c.rank != rank2 &&
          c != card1
      )
    } yield Hand(card1 :: card2 :: pair.toList ++ set.toList)

  val genFlushHand: Gen[Hand] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    flush <- pick(5, suited)
    card1 <- genCard.suchThat { c =>
      !suited.contains(c)
    }
    card2 <- genCard.suchThat { c =>
      !suited.contains(c) &&
      c != card1
    }
  } yield Hand(card1 :: card2 :: flush.toList)

  val genNonFlushHand: Gen[Hand] = for {
    suit <- genSuit
    suited = Deck.all.filter(_.suit == suit)
    fourFlush <- pick(4, suited)
    card1 <- genCard.suchThat { c =>
      !suited.contains(c)
    }
    card2 <- genCard.suchThat { c =>
      !suited.contains(c) &&
      c != card1
    }
    card3 <- genCard.suchThat { c =>
      !suited.contains(c) &&
      c != card1 &&
      c != card2
    }
  } yield Hand(card1 :: card2 :: card3 :: fourFlush.toList)

  /// group card by rank; pick a rank between A-5
  // take a card from that top rank and 4 ranks beneath it.
  // 5 is a special case
  // 0-2, 1-3,2-4,3-5,4-6,5-7,6-8,7-9,8-10,9-11,10-12,11-12,12-14

  val genStraightHand: Gen[Hand] = {
    val grouped = Deck.all.groupBy(_.rank).toList.sortBy(_._1.value)
    for {
      rank <- genRank.suchThat(_.value >= 5)
      highSlice: List[(Rank, List[Card])] =
        grouped.slice(rank.value - 6, rank.value - 1)
      lowSlice: List[(Rank, List[Card])] =
        grouped.last :: grouped.slice(rank.value - 5, rank.value - 1)
      hand <-
        if (rank.value > 5) genStraightHand_(highSlice)
        else genStraightHand_(lowSlice)
    } yield hand
  }

  private def genStraightHand_(slice: List[(Rank, List[Card])]): Gen[Hand] =
    for {
      c1 <- Gen.oneOf(slice(0)._2)
      c2 <- Gen.oneOf(slice(1)._2)
      c3 <- Gen.oneOf(slice(2)._2)
      c4 <- Gen.oneOf(slice(3)._2)
      c5 <- Gen.oneOf(slice(4)._2)
      n1 <- genCard.suchThat(c => !List(c1, c2, c3, c4, c5).contains(c))
      n2 <- genCard.suchThat(c => !List(c1, c2, c3, c4, c5, n1).contains(c))
    } yield Hand(List(c1, c2, c3, c4, c5, n1, n2))

  property("FourOfAKind must have 4 cards of same rank") = forAll(genFourOfAKind) { hand =>
    (Ranking(hand) == FourOfAKind) ==> {
      hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
        cards.length == 4
      }
    }
  }

  property("a hand is 4 of a kind is FourOfAKind and NOT ThreeOfAKind") = forAll(genFourOfAKind) { hand =>
    (Ranking(hand) == FourOfAKind) ++ (Ranking(hand) != ThreeOfAKind)
  }

  property("3 cards of the same rank is ThreeOfAKind") = forAll(genThreeOfAKind) { hand =>
    (Ranking(hand) != FullHouse) ++ (Ranking(hand) != Flush) ++
      (Ranking(hand) != Straight) ==> {
        (Ranking(hand) != FourOfAKind) ++ (Ranking(hand) ?= ThreeOfAKind)
      }
  }
  // later on the user will need to be able to choose cards
  // so I might have to implement that for this test...

  property("a FullHouse has 4 or less ranks") = forAll(genFullHouse) { hand: Hand =>
    hand.cards.groupBy(_.rank).size <= 4
  }

  property("5 or more cards of a suit is a Flush") = forAll(genFlushHand, genNonFlushHand) {
    (flushHand, nonFlushHand) =>
      (Ranking(flushHand) ?= Flush) && Ranking(nonFlushHand) != Flush
  }

  property("a Flush hand has 3 or less suits") = forAll(genFlushHand) { hand =>
    hand.cards.groupBy(c => c.suit).size <= 3
  }

  property("sequential cards rank a Straight") = forAll(genStraightHand) { hand =>
    (Ranking(hand) != Flush) ==> {
      Ranking(hand) ?= Straight
    }
  }

  // TODO test for straight flush

}
