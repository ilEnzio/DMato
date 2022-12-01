package poker

import cats._
import cats.data.State
import cats.effect.IO
import cats.effect.std._
import cats.implicits.toFunctorOps
import cats.syntax.all._
import poker.Street.Preflop

case class Deck(cards: List[Card]) {
  def dealFlop: (FlopCards, Deck) =
    cards match {
      case fst :: snd :: thr :: deck =>
        (FlopCards(fst, snd, thr), Deck(deck))
    }

  def dealTurn: (TurnCard, Deck) =
    cards match {
      case h :: deck => (TurnCard(h), Deck(deck))
    }

  def dealRiver: RiverCard =
    cards match {
      case h :: _ => (RiverCard(h))
    }
}

object Deck {

  trait StartingDeck {
    def shuffle[F[_]: Functor: Random]: F[StartingDeck]
    def dealHoleCards(numPlayers: Int): State[Deck, Preflop]
  }
  final private case class StartingDeckImpl(cards: List[Card])
      extends StartingDeck {
    override def shuffle[F[_]: Functor: Random]: F[StartingDeck] =
      Random[F].shuffleList(cards).map(StartingDeckImpl.apply)

    // StartingDeck(Shuffle, Dealhole cards) => PreflopDeck => FlopDeck => TurnDeck => [RiverBoard]

    def shuffle2 = IO(scala.util.Random.shuffle(all))

    override def dealHoleCards(numPlayers: Int): State[Deck, Preflop] = {
      // TODO Erg Can't figure this out yet.
      //        val shuffledDeck =
      //          shuffle[F[StartingDeck]] //IO(Random.shuffle(StartingDeck.all))
      for {
        shuffledDeck <- State.get[Deck]
        numHoleCards = numPlayers * 2
        /// TODO refactor this
        players = shuffledDeck.cards
          .take(numHoleCards)
          .grouped(2)
          .zip(for (x <- 1 to numPlayers) yield x)
          .map { case (List(y, z), Position.positionMap(x)) => Player(x, y, z) }
        .toList
        _ <- State.set(Deck(cards.drop(numHoleCards)))
      } yield Preflop(players)
    }
  }

  private def startingDeckImpl: StartingDeckImpl = {
    val cardList = for {
      rank <- Rank.all
      suit <- Suit.all
    } yield Card(rank, suit)
    StartingDeckImpl(cardList)
  }

  def startingDeck: StartingDeck = startingDeckImpl
  val all: List[Card]            = startingDeckImpl.cards

}

final case class FlopCards(card1: Card, card2: Card, card3: Card)
final case class TurnCard(card: Card)
final case class RiverCard(card: Card)
