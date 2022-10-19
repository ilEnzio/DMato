package poker

import cats.effect.IO
import Deck._
import cats.effect.unsafe.implicits.global

import scala.util.Random

// the idea here this that each "state" is a final case class
// so the state is going through pipeline
// then I can somehow use unapply methods
/// TODO: I don't understand the proper use of unapply.
/// I feel like I've just added a bunch of boiler plate

sealed trait Street {
  val allHands: List[Hand]
  val players: List[Player]
} // Street

object Street {

  def dealHoleCards(numPlayers: Int): IO[Preflop] = {

    // TODO Something isn't right here; Find out through test
    val shuffledDeck = IO(Random.shuffle(StartingDeck.all))
    val numHoleCards = numPlayers * 2
    for {
      cards <- shuffledDeck
      players = cards
        .take(numHoleCards)
        .grouped(2)
        .map { case List(x, y) => Player(x, y) } // TODO unsafe
        .toList
    } yield Preflop(players, PreFlopDeckImpl(cards.drop(numHoleCards))) // TODO I've broken encapuslation here

//    deck.map(Preflop(players, _))
  }

  final case class Preflop(players: List[Player], deck: PreflopDeck) extends Street {

    val allHands: List[Hand] =
      players.map { case Player(x, y) => Hand.rank(List(x, y)) }
  }

//

  final case class Flop(players: List[Player], deck: FlopDeck, card1: Card, card2: Card, card3: Card) extends Street {
    val allHands: List[Hand] =
      players.map { case Player(x, y) => Hand.rank(List(x, y, card1, card2, card3)) }
  }

  final case class Turn(players: List[Player], deck: TurnDeck, card1: Card, card2: Card, card3: Card, turn: Card)
      extends Street {
    val allHands: List[Hand] =
      players.map { case Player(x, y) => Hand.rank(List(x, y, card1, card2, card3, turn)) }
  }

  final case class River(
    players: List[Player],
    card1: Card,
    card2: Card,
    card3: Card,
    turn: Card,
    river: Card
  ) extends Street {
    val allHands: List[Hand] =
      players.map { case Player(x, y) => Hand.rank(List(x, y, card1, card2, card3, turn, river)) }
  }

  /// State machine needs to go to the Deck. (FlopCards, FlopDeck)
  // Flop - street
  // FlopCards - the three cards
  def dealFlop(preflop: Preflop): Flop = { // (FlopCards, FlopDeck)

    val (flop, flopDeck) = preflop.deck.dealFlop
    Flop(preflop.players, flopDeck, flop.card1, flop.card2, flop.card3)
  }

  def dealTurn(flop: Flop): Turn = {
    val (turn, turnDeck) = flop.deck.dealTurn

    Turn(flop.players, turnDeck, flop.card1, flop.card2, flop.card3, turn.card)
  }

  def dealRiver(turn: Turn): River = {
    val river = turn.deck.dealRiver
    River(turn.players, turn.card1, turn.card2, turn.card3, turn.turn, river.card)
  }

}

case class Player(card1: Card, card2: Card) // position ??
