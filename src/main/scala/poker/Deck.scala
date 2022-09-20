package poker

import cats.effect.IO

import scala.util.Random

case class Deck(cards: List[Card]) {
  def size: Int = cards.length
  // I'm starting to get uncomfortable that outside stuff can reach into
  // cards.  Not sure about this.
  //

  // TODO - Create some compensating action tests for these
  def add(card: Card): Deck                = Deck(card +: cards)
  def add(cardList: List[Card]): Deck      = Deck(cards ++ cardList)
  def take(n: Int): List[Card]             = cards.take(n)
  def drop(n: Int): Deck                   = Deck(cards.drop(n))
  def remove(card: Card): Deck             = Deck(cards.filterNot(_ == card))
  def remove(otherCards: List[Card]): Deck = Deck(cards.filterNot(otherCards.contains(_)))
}

object Deck {

  final private case class Impl(cards: List[Card]) extends PreflopDeck {
    override def dealFlop: (FlopCards, FlopDeck) =
  }
  def shuffle: IO[PreflopDeck] = IO(Impl(Random.shuffle(startingDeckImpl.cards)))

  private def startingDeckImpl: Impl = {
    val cardList = for {
      rank <- Rank.all
      suit <- Suit.all
    } yield Card(rank, suit)
    Impl(cardList)
  }

  def startingDeck: PreflopDeck = startingDeckImpl

  val all: List[Card] = startingDeckImpl.cards

}

sealed trait PreflopDeck {
  def dealFlop: (FlopCards, FlopDeck) // (FlopCards, FlopDeck)
}

sealed trait FlopDeck {
  def dealTurn: Unit // (FlopCards, TurnCard, TurnDeck)
}

sealed trait TurnDeck {
  def dealRiver: Unit // (FlopCards, TurnCard, RiverCard, FlopDeck)
}

final case class FlopCards(card1: Card, card2: Card, card3: Card)
final case class TurnCard(card: Card)
final case class RiveCard(card: Card)
