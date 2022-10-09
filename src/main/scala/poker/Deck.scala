package poker

import cats.effect.IO

import scala.util.Random

case class Deck(cards: List[Card]) {
  def size: Int = cards.length

// I'm starting to get uncomfortable that outside stuff can reach into
// cards.  Not sure about this.
// TODO I think I can get rid of all this
// TODO - Create some compensating action tests for these
  def add(card: Card): Deck                = Deck(card +: cards)
  def add(cardList: List[Card]): Deck      = Deck(cards ++ cardList)
  def take(n: Int): List[Card]             = cards.take(n)
  def drop(n: Int): Deck                   = Deck(cards.drop(n))
  def remove(card: Card): Deck             = Deck(cards.filterNot(_ == card))
  def remove(otherCards: List[Card]): Deck = Deck(cards.filterNot(otherCards.contains(_)))
}

object Deck {

  trait PreflopDeck {
    def dealFlop: (FlopCards, FlopDeck)
  }

  final private case class PreFlopImpl(cards: List[Card]) extends PreflopDeck {
    override def dealFlop: (FlopCards, FlopDeck) =
      cards match {
        case fst :: snd :: thr :: deck => (FlopCards(fst, snd, thr), FlopImpl(deck))
      }
  }

  trait FlopDeck {
    def dealTurn: (TurnCard, TurnDeck)
  }

  final private case class FlopImpl(cards: List[Card]) extends FlopDeck {
    override def dealTurn: (TurnCard, TurnDeck) =
      cards match {
        case h :: deck => (TurnCard(h), TurnImpl(deck))
      }
  }

  trait TurnDeck {
    def dealRiver: RiverCard
  }

  final private case class TurnImpl(cards: List[Card]) extends TurnDeck {
    override def dealRiver: RiverCard =
      cards match {
        case h :: _ => (RiverCard(h))
      }
  }
  object PreflopDeck {

    def shuffle: IO[PreflopDeck] = IO(PreFlopImpl(Random.shuffle(startingDeckImpl.cards)))

    private def startingDeckImpl: PreFlopImpl = {
      val cardList = for {
        rank <- Rank.all
        suit <- Suit.all
      } yield Card(rank, suit)
      PreFlopImpl(cardList)
    }

    def startingDeck: PreflopDeck = startingDeckImpl

    val all: List[Card] = startingDeckImpl.cards

  }

}

final case class FlopCards(card1: Card, card2: Card, card3: Card)
final case class TurnCard(card: Card)
final case class RiverCard(card: Card)
