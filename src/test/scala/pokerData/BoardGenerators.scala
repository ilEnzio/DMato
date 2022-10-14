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

  val genNumberOfPlayers: Gen[Int] =
    choose(2, 10)
  val genPreflopBoard: Gen[Preflop] = {

    for {
      numPlayers <- genNumberOfPlayers
    } yield dealHoleCards(numPlayers).unsafeRunSync()

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
