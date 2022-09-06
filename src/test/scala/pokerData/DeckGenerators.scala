package pokerData

import org.scalacheck.Gen
import poker.Deck.makeStartingDeck
import poker.{Card, Deck, Rank, Suit}

object DeckGenerators {
  val genSuit: Gen[Suit] = Gen.oneOf(Suit.all)
  val genRank: Gen[Rank] = Gen.oneOf(Rank.all)
  val genCard: Gen[Card] = for {
    rank <- genRank
    suit <- genSuit
  } yield Card(rank, suit)

  val startingDeck: Deck = makeStartingDeck //
  val genDeck: Gen[Deck] =
    Gen.const(makeStartingDeck)
}
