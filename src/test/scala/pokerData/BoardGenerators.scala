package pokerData

import cats.effect.unsafe.implicits.global
import org.scalacheck.Gen.choose
import org.scalacheck.Gen
import poker.Deck.PreflopDeck
import poker.Street
import poker.Street.{deal, dealHoleCards, Flop, Preflop, River, Turn}
//import poker.Deck.makeStartingDeck
import poker.Deck.PreflopDeck._

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
//    val testDeck: PreflopDeck = PreflopDeck.shuffle.unsafeRunSync()
    Street.dealHoleCards(numPlayers).unsafeRunSync()
//    Preflop(???, testDeck)
  }

  val genFlopBoard: Gen[Flop] = {
    for {
      preflop <- genPreflopBoard
    } yield deal(preflop).asInstanceOf[Flop]

  }

  val genTurnBoard: Gen[Turn] = {
    for {
      flop <- genFlopBoard
    } yield deal(flop).asInstanceOf[Turn]

  }

  val genRiverBoard: Gen[River] = {
    for {
      turn <- genTurnBoard
    } yield deal(turn).asInstanceOf[River]
  }

}
