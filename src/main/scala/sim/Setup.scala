package sim

import cats._
import cats.effect.std.Random
import cats.syntax.all._
import equity.EquityService.getOrDeal
import poker.{Card, Position, Rank, Suit}

final case class Setup private (
  // Do I even need a deck? The deck basically gets derived
  // from the cards that are prepopulated by the sim
  //  deck: F[SimDeck],
  players: List[
    Player
  ],
  // TODO Should I type these to make subsequent code more expressive?
  card1: Option[Card],
  card2: Option[Card],
  card3: Option[Card],
  turn: Option[Card],
  river: Option[Card]
) {

  //  def mapK[G[_]](f: F[Card] => G[Card]): SimSetup[G] = ???

}

object Setup {

  // TODO There needs to be validation on the the SimSetup - in other words
  // you shouldn't be able to have an illegal SimSetup
  // Every card should be distinct and there should be at least 2 players.

  def validate[F[_]](Deck: Setup): F[Unit] = ???
  //
  //  def applyK(
  //    players: List[SimPlayer],
  //    card1: Option[Card],
  //    card2: Option[Card],
  //    card3: Option[Card],
  //    turn: Option[Card],
  //    river: Option[Card]
  //  ): SimSetup[Id] = {
  //
  //  }
}
//white space

final case class Player private (
  position: Position,
  card1: Option[Card],
  card2: Option[Card]
) {

  // TODO not sure how to use this yet.
  //  def mapK[G[_]](f: F[Card] => G[Card]): SimPlayer[G] =
  //    copy(card1 = f(card1), card2 = f(card2))  ???

}
object Player {
  // TODO refactor??
  // This is so strange... why would a player method return a deck???
  def applyK(
    position: Position,
    card1: Option[Card],
    card2: Option[Card]
  )(deck: Deck): (Player, Deck) = {

    val getOrDealPlayerCards = for {
      cardA <- getOrDeal(card1)
      cardB <- getOrDeal(card2)
    } yield Player(position, cardA.some, cardB.some)
    getOrDealPlayerCards.run(deck).value.swap
  }
}

final case class Deck(cards: List[Card]) {

  // TODO this isnt being used yet.
  def shuffle[F[_]: Functor: Random]: F[Deck] =
    Random[F].shuffleList(cards).map(Deck)

  private def startingDeckImpl: Deck = {
    val cards = for {
      rank <- Rank.all
      suit <- Suit.all
    } yield Card(rank, suit)
    Deck(cards)
  }

}

final case class Result(winnersList: List[Position]) {}
