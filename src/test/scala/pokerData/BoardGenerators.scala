package pokerData

import cats.effect.unsafe.implicits.global
import org.scalacheck.Gen.choose
import org.scalacheck.Gen
import poker.Street.{deal, Flop, Preflop, Turn}
//import poker.Deck.makeStartingDeck
import poker.{Player, PreflopDeck}

object BoardGenerators {

  val genPreflopBoard: Gen[Preflop] = {
    // Deck
    // 2-9 players
    // for each player take 2 cards from the deck
    val numPlayers = choose(2, 9).sample.get
//    val deck       = Preflop(2)
//    val cards      = deck.take(numPlayers * 2)
//    val newDeck    = deck.drop(numPlayers * 2)
//    // TODO still not safe!!
//    val players = cards.grouped(2).map { case h :: t => Player(h, t.headOption.get) }.toList

    Preflop(numPlayers)
  }

  val genFlopBoard: Gen[Flop] = {
    for {
      preflop <- genPreflopBoard
//      newDeck     = preflop.deck.drop(3)
//      f :: s :: t = preflop.deck.take(3)

    } yield deal(preflop).asInstanceOf[Flop]

  }

  val genTurnBoard: Gen[Turn] = {
    for {
      flop <- genFlopBoard
//      newDeck = flop.deck.drop(1)
//      turn    = flop.deck.take(1)
    } yield deal(flop).asInstanceOf[Turn]

  }

}
