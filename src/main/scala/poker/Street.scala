package poker

import cats.effect.IO
import Deck._

// the idea here this that each "state" is a final case class
// so the state is going through pipeline
// then I can somehow use unapply methods
/// TODO: I don't understand the proper use of unapply.
/// I feel like I've just added a bunch of boiler plate

sealed trait Street {
  val allHands: List[Hand]
} // Street

object Street {

  def deal(numPlayers: Int): IO[Street] = {

    // TODO Something isn't right here; Find out through test

    val players = PreflopDeck.all
      .take(numPlayers * 2)
      .grouped(2)
      .map { case List(x, y) => Player(x, y) }
      .toList
//    new Preflop(players, PreflopDeck.Impl(PreflopDeck.all.drop(numPlayers * 2)))
    val deck: IO[PreflopDeck] = PreflopDeck.shuffle
//    for {
//      nd <- deck
//    } yield ???
    deck.map(Preflop(players, _))
  }

  //TODo: come back and fix the unapply
  def deal(boardState: Street): Street =
    boardState match {
      //      case Preflop(ps, d)                        => dealFlop(ps, d)
      //      case BoardState.Flop(ps, d, c1, c2, c3)    => dealTurn(ps, d, c1, c2, c3)
      //      case BoardState.Turn(ps, d, c1, c2, c3, t) => dealRiver(ps, d, c1, c2, c3, t)
      //      case BoardState.River(ps, d, c1, c2, c3, t, r) => ???
      case x: Preflop => dealFlop(x.players, x.deck) // flop factory
      case x: Flop    => dealTurn(x.players, x.deck, x.card1, x.card2, x.card3)
      case x: Turn    => dealRiver(x.players, x.deck, x.card1, x.card2, x.card3, x.turn)
      case x: River   => x
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
  def dealFlop(players: List[Player], deck: PreflopDeck): Flop = { // (FlopCards, FlopDeck)

    val (flop, flopDeck) = deck.dealFlop
    Flop(players, flopDeck, flop.card1, flop.card2, flop.card3)
  }

  def dealTurn(players: List[Player], deck: FlopDeck, fl1: Card, fl2: Card, fl3: Card): Turn = {

    val (turn, turnDeck) = deck.dealTurn

    Turn(players, turnDeck, fl1, fl2, fl3, turn.card)
  }

  def dealRiver(players: List[Player], deck: TurnDeck, fl1: Card, fl2: Card, fl3: Card, t: Card): River = {

    val river = deck.dealRiver
    River(players, fl1, fl2, fl3, t, river.card)
  }

}

case class Player(card1: Card, card2: Card) // position ??
