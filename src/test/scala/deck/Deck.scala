package deck

import org.scalacheck.{Arbitrary, Gen}

case class Deck(cards: List[Card]) {
  def size: Int = cards.length

}

object Deck {

  val genSuit = Gen.oneOf(Suit.all)
  val genRank = Gen.oneOf(Rank.all)
  val genCard = for {
    rank <- genRank
    suit <- genSuit
  } yield Card(rank, suit)

  def make: Deck = {
    val cardList = (for (_ <- 0 until 52) yield genCard.sample.get).toList
    Deck(cardList)
  }

}
