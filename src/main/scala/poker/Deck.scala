package poker

import cats._
import cats.effect.IO
import cats.effect.kernel.Sync
import cats.effect.std.Random
import cats.syntax.all._
import poker.Street.Preflop
import cats.effect.unsafe.implicits.global

case class Deck(cards: List[Card]) {}

object Deck {

  trait StartingDeck {
    def shuffle[F[_]: Functor: Random: Sync]: F[List[Card]]
    def dealHoleCards[F[_]: Functor: Random: Sync](
      numPlayers: Int
    ): F[Preflop]
  }
  final private case class StartingDeckImpl(cards: List[Card])
      extends StartingDeck {
    override def shuffle[F[_]: Functor: Random: Sync]: F[List[Card]] =
      for {
        x             <- Random.scalaUtilRandom[F]
        shuffledCards <- x.shuffleList(cards)
      } yield shuffledCards
//      Random[F].shuffleList(cards).map(StartingDeckImpl.apply)

    // StartingDeck(Shuffle, Dealhole cards) => PreflopDeck => FlopDeck => TurnDeck => [RiverBoard]

//    def shuffle2 = IO(scala.util.Random.shuffle(all))
    override def dealHoleCards[F[_]: Functor: Random: Sync](
      numPlayers: Int
    ): F[Preflop] = {
      // TODO Erg Can't figure this out yet.
      //        val shuffledDeck =
      //          shuffle[F[StartingDeck]] //IO(Random.shuffle(StartingDeck.all))
//      val shuffledDeck = shuffle2
      val numHoleCards = numPlayers * 2
      for {
        shuffledCards <- shuffle[F]
        /// TODO refactor this
        players = shuffledCards
          .take(numHoleCards)
          .grouped(2)
          .zip(for (x <- 1 to numPlayers) yield x)
          .map { case (List(y, z), Position.positionMap(x)) => Player(x, y, z) }
          .toList
      } yield Preflop(
        players,
        PreFlopDeckImpl(shuffledCards.drop(numHoleCards))
      )
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

trait PreflopDeck {
  def dealFlop: (FlopCards, FlopDeck)
}

final private case class PreFlopDeckImpl(cards: List[Card])
    extends PreflopDeck {
  override def dealFlop: (FlopCards, FlopDeck) =
    cards match {
      case fst :: snd :: thr :: deck =>
        (FlopCards(fst, snd, thr), FlopImpl(deck))
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

final case class FlopCards(card1: Card, card2: Card, card3: Card)
final case class TurnCard(card: Card)
final case class RiverCard(card: Card)
