package poker

import cats.effect.IO

import scala.util.Random

case class Deck(cards: List[Card]) {
  def size: Int = cards.length
// I'm starting to get uncomfortable that outside stuff can reach into
// cards.  Not sure about this.

// TODO - Create some compensating action tests for these
  def add(card: Card): Deck                = Deck(card +: cards)
  def add(cardList: List[Card]): Deck      = Deck(cards ++ cardList)
  def take(n: Int): List[Card]             = cards.take(n)
  def drop(n: Int): Deck                   = Deck(cards.drop(n))
  def remove(card: Card): Deck             = Deck(cards.filterNot(_ == card))
  def remove(otherCards: List[Card]): Deck = Deck(cards.filterNot(otherCards.contains(_)))
}
trait PreflopDeck {
  def dealFlop: (FlopCards, List[Card])
}

object PreflopDeck {
// TODO I don't understand how to get this to work while it's private
  final case class Impl(cards: List[Card]) extends PreflopDeck {
    override def dealFlop: (FlopCards, List[Card]) =
      cards match {
        case fst :: snd :: thr :: deck => (FlopCards(fst, snd, thr), deck)
      }
    def size: Int = cards.size
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
  val all: List[Card]           = startingDeckImpl.cards

}
trait FlopDeck {
  def dealTurn: (TurnCard, List[Card])
}

object FlopDeck {
  final case class Impl(cards: List[Card]) extends FlopDeck {
    override def dealTurn: (TurnCard, List[Card]) =
      cards match {
        case h :: deck => (TurnCard(h), deck)
      }
  }

}

trait TurnDeck {
  def dealRiver: RiverCard
}

object TurnDeck {
  // TODO: I don't know how to access these when they are private
  final case class Impl(cards: List[Card]) extends TurnDeck {
    override def dealRiver: RiverCard =
      cards match {
        case h :: _ => (RiverCard(h))
      }
  }
}

final case class FlopCards(card1: Card, card2: Card, card3: Card)
final case class TurnCard(card: Card)
final case class RiverCard(card: Card)
