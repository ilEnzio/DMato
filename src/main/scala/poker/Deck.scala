package poker

import cats._
import cats.effect.std._
import cats.syntax.all._

case class Deck(cards: List[Card]) {
//  def size: Int = cards.length

// I'm starting to get uncomfortable that outside stuff can reach into
// cards.  Not sure about this.
// TODO I think I can get rid of all this
// TODO - Create some compensating action tests for these
//  def add(card: Card): Deck                = Deck(card +: cards)
//  def add(cardList: List[Card]): Deck      = Deck(cards ++ cardList)
//  def take(n: Int): List[Card]             = cards.take(n)
//  def drop(n: Int): Deck                   = Deck(cards.drop(n))
//  def remove(card: Card): Deck             = Deck(cards.filterNot(_ == card))
//  def remove(otherCards: List[Card]): Deck = Deck(cards.filterNot(otherCards.contains(_)))
}

object Deck {

  trait StartingDeck {
    def shuffle[F[_]: Functor: Random]: F[StartingDeck]
  }

  final private case class StartingDeckImpl(cards: List[Card]) extends StartingDeck {
    override def shuffle[F[_]: Functor: Random]: F[StartingDeck] =
      Random[F].shuffleList(cards).map(StartingDeckImpl.apply)
  }

  object StartingDeck { // TODO flatten nesting?
//    def shuffle: IO[StartingDeck] =

    private def startingDeckImpl: StartingDeckImpl = {
      val cardList = for {
        rank <- Rank.all
        suit <- Suit.all
      } yield Card(rank, suit)
      StartingDeckImpl(cardList)
    }

    def startingDeck: StartingDeck = startingDeckImpl

    val all: List[Card] = startingDeckImpl.cards

  }

  trait PreflopDeck {
    def dealFlop: (FlopCards, FlopDeck)
  }
// TODO I've broken encapsulation here.
  final case class PreFlopDeckImpl(cards: List[Card]) extends PreflopDeck {
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

//    def shuffle: IO[PreflopDeck] = IO(PreFlopImpl(Random.shuffle(startingDeckImpl.cards)))
//
//    private def startingDeckImpl: PreFlopImpl = {
//      val cardList = for {
//        rank <- Rank.all
//        suit <- Suit.all
//      } yield Card(rank, suit)
//      PreFlopImpl(cardList)
//    }
//
//    def startingDeck: StartingDeck = startingDeckImpl
//
//    val all: List[Card] = startingDeckImpl.cards

  }

}

final case class FlopCards(card1: Card, card2: Card, card3: Card)
final case class TurnCard(card: Card)
final case class RiverCard(card: Card)
