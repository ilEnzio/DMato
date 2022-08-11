package Ranking

import deck.{Deck, Hand, Rank}
import deck.DeckPropTest.{genCard, genRank}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

object RankingTest extends Properties("RankingTests") {

//  val handGen = genDeck.
// Seq.combinations function returns combinations of subsequences
  // so I should be able to use this to naively determine straights
  // order the cards this look for

  // a poker hand is 5 cards.  but I think for the purposes of this
  // it is the best 5 cards of 7, and the rank of the hand is determined by
  // the five cards
  val genHand = Hand(Deck.makeStartingDeck.shuffle.cards.take(7))

  implicit val arbHand = Arbitrary(genHand)

  property("a hand is 4 of a kind") = forAll(genRank) { rank =>
    val (partialHand, remDeck) =
      (Deck.makeStartingDeck.cards.filter(_.rank == rank), Deck(Deck.makeStartingDeck.cards.filterNot(_.rank == rank)))
    val finalHand: Hand = Hand(partialHand ++ remDeck.shuffle.cards.take(3))
    finalHand.cards.count(_.rank == rank) == 4

  }

  property("3 of a kind is NOT 4 of a kind") = forAll(genRank) { rank1 =>
    val (fourCards, remDeck) =
      (
        Deck.makeStartingDeck.cards.filter(_.rank == rank1),
        Deck(Deck.makeStartingDeck.cards.filterNot(_.rank == rank1))
      )
    val threeOfAKind = fourCards.tail
    val rank2        = Gen.oneOf(Rank.all.filter(_ != rank1)).sample.get
    val (altFour, remDeck2) =
      (
        remDeck.cards.filter(_.rank == rank2),
        Deck(remDeck.cards.filterNot(_.rank == rank1))
      )
    val badFinalHand: Hand = Hand(altFour.head :: threeOfAKind ++ remDeck2.shuffle.cards.take(3))
    badFinalHand.cards.count(_.rank == rank1) != 4 && badFinalHand.cards.count(_.rank == rank1) == 3

  }
  // later on the user will need to be able to choose cards
  // so I might have to implement that for this test...

}
