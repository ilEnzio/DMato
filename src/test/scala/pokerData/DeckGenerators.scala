package pokerData

import org.scalacheck.{Arbitrary, Gen}
import poker.{Card, Deck, Rank, Suit}

object DeckGenerators {
  val genSuit: Gen[Suit] = Gen.oneOf(Suit.all)
  val genRank: Gen[Rank] = Gen.oneOf(Rank.all)
  implicit val arbRank   = Arbitrary(genRank)
  val genCard: Gen[Card] = for {
    rank <- genRank
    suit <- genSuit
  } yield Card(rank, suit)
  implicit val arbCard = Arbitrary(genCard)
// TODO - Do I even need either of these?

//  val startingDeck: Deck = makeStartingDeck //
//  val genDeck: Gen[Deck] =
//    Gen.const(makeStartingDeck)
}
