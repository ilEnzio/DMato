package pokerData

import cats.effect.unsafe.implicits.global
import org.scalacheck.Gen.choose
import org.scalacheck.Gen
import poker.BoardState.{Flop, Preflop, Turn}
import poker.Deck.makeStartingDeck
import poker.Player

object BoardGenerators {

  val genPreflop: Gen[Preflop] = {
    // Deck
    // 2-9 players
    // for each player take 2 cards from the deck
    val numPlayers = choose(2, 9).sample.get
    val deck       = makeStartingDeck.shuffle.unsafeRunSync()
    val cards      = deck.take(numPlayers * 2)
    val newDeck    = deck.drop(numPlayers * 2)
    // TODO still not safe!!
    val players = cards.grouped(2).map { case h :: t => Player(h, t.headOption.get) }.toList

    Preflop(players, newDeck)
  }

  val genFlop: Gen[Flop] = {
    for {
      preflop <- genPreflop
      newDeck     = preflop.deck.drop(3)
      f :: s :: t = preflop.deck.take(3)
    } yield Flop(preflop.players, newDeck, f, s, t.headOption.get)

  }

  val genTurn: Gen[Turn] = {
    for {
      flop <- genFlop
      newDeck = flop.deck.drop(1)
      turn    = flop.deck.take(1)
    } yield Turn(flop.players, newDeck, flop.card1, flop.card2, flop.card3, turn.headOption.get)

  }

}
