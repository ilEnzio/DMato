package deck

import scala.util.Random

case class Deck(cards: List[Card]) {
  def size: Int = cards.length
  // I'm starting to get uncomfortable that outside stuff can reach into
  // cards.  Not sure about this.
  def shuffle: Deck                   = Deck(Random.shuffle(this.cards))
  def add(card: Card): Deck           = Deck(card +: cards)
  def add(cardList: List[Card]): Deck = Deck(cards ++ cardList)
}

object Deck {

  def makeStartingDeck: Deck = {
    val cardList = for {
      rank <- Rank.all
      suit <- Suit.all
    } yield Card(rank, suit)
    Deck(cardList)
  }

  val all: List[Card] = makeStartingDeck.cards

}
