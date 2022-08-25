package poker

import cats.effect.IO

import scala.util.Random

sealed trait BoardState {
  // players/players hands from 2 - 9
  // board cards 0 - 5
  // deck(remaining cards)
  val players: List[Player]
  val board: List[Card]
  val deck: Deck
}

object BoardState {
  def deal(players: List[Player]): BoardState = {
    val board: List[Card] = Nil
    val deck: Deck        = ???
    Preflop(players, board, deck)
  }
  def deal(street: BoardState): BoardState = street match {
    case x: Preflop => Flop(x.players, ???, ???)
    case x: Flop    => Turn(x.players, ???, ???)
    case x: Turn    => River(x.players, ???, ???)
    case x: River   => x
  }
  def shuffleDeck(deck: Deck): IO[Deck] =
    IO(Deck(Random.shuffle(deck.cards)))

  // TODO - Create some compensating action tests for these
  def add(card: Card, deck: Deck): Deck           = deck.copy(cards = card :: deck.cards)
  def add(cardList: List[Card], deck: Deck): Deck = deck.copy(cards = cardList ++ deck.cards)
  def take(n: Int, deck: Deck): (List[Card])      = deck.cards.take(n)
  def drop(n: Int, deck: Deck): Deck              = deck.copy(cards = deck.cards.drop(n))

}
case class Preflop(players: List[Player], board: List[Card], deck: Deck) extends BoardState {}
case class Flop(players: List[Player], board: List[Card], deck: Deck)    extends BoardState
case class Turn(players: List[Player], board: List[Card], deck: Deck)    extends BoardState
case class River(players: List[Player], board: List[Card], deck: Deck)   extends BoardState

case class Player(holeCards: (Card, Card)) {}
