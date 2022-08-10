package deck

import scala.util.Random

case class Deck(cards: List[Card]) {
  def size: Int = cards.length

}

object Deck {

  def makeStartingDeck: Deck = {
    val cardList = for {
      rank <- Rank.all
      suit <- Suit.all
    } yield Card(rank, suit)
    Deck(cardList)
  }

  def all: List[Card]           = makeStartingDeck.cards
  def shuffle(deck: Deck): Deck = Deck(Random.shuffle(deck.cards))

}
