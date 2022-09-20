package poker

import cats.effect.IO

// the idea here this that each "state" is a final case class
// so the state is going through pipeline
// then I can somehow use unapply methods
/// TODO: I don't understand the proper use of unapply.
/// I feel like I've just added a bunch of boiler plate

sealed trait BoardState // Street

object BoardState {

  def deal(players: List[Player]): IO[BoardState] = {
    val deck: IO[PreflopDeck] = Deck.shuffle
    deck.map(Preflop(players, _))
  }

  //TODo: come back and fix the unapply
  def deal(boardState: BoardState): BoardState =
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

  final case class Preflop(players: List[Player], deck: PreflopDeck) extends BoardState {
    def allHands: List[Hand] =
      players.map { case Player(x, y) => Hand.rank(List(x, y)) }
  }
  final case class Flop(players: List[Player], deck: Deck, card1: Card, card2: Card, card3: Card) extends BoardState {
    def allHands: List[Hand] =
      players.map { case Player(x, y) => Hand.rank(List(x, y, card1, card2, card3)) }
  }
  final case class Turn(players: List[Player], deck: Deck, card1: Card, card2: Card, card3: Card, turn: Card)
      extends BoardState
  final case class River(
    players: List[Player],
    deck: Deck, // ToDo remove
    card1: Card,
    card2: Card,
    card3: Card,
    turn: Card,
    river: Card
  ) extends BoardState
  //// TODo Refactor all the unapplys
  // TODO none of this is safe??

  /// State machine needs to go to the Deck. (FlopCards, FlopDeck)
  // Flop - street
  // FlopCards - the three cards
  def dealFlop(players: List[Player], deck: PreflopDeck): BoardState.Flop = { // (FlopCards, FlopDeck)
    val newDeck = deck.drop(3)
    val flop    = deck.take(3)
    Flop(players, newDeck, flop(0), flop(1), flop(2))
  }

  def dealTurn(players: List[Player], deck: Deck, fl1: Card, fl2: Card, fl3: Card): Turn = {
    val newDeck = deck.drop(1)
    val turn    = deck.take(1)
    Turn(players, newDeck, fl1, fl2, fl3, turn.headOption.get)
  }

  def dealRiver(players: List[Player], deck: Deck, fl1: Card, fl2: Card, fl3: Card, t: Card): River = {
    val newDeck = deck.drop(1)
    val river   = deck.take(1)
    River(players, newDeck, fl1, fl2, fl3, t, river.headOption.get)
  }

}

case class Player(card1: Card, card2: Card) // position ??
