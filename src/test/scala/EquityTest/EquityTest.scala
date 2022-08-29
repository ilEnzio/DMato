package EquityTest

import cats.effect.unsafe.implicits.global
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties
import poker.{BoardState, Deck, Player, Preflop}
import poker.OrderInstances._
import test.pokerData.DataGenerators._

object EquityTest extends Properties("Equity Tests") {

  /// given a preflop board, and x  simulations
// each hand as a certain amount of equity
// the percentage of times it won
//  property("preflop equity of Pair vs Pair") = forAll(genPair, genPair) { (overPair, underPair) =>
//    (pairOrder.compare(overPair, underPair) > 0) ==> {
//
//      val pl1            = Player(overPair.cards.head, overPair.cards(1))
//      val pl2            = Player(underPair.cards.head, underPair.cards(1))
//
//      val startDeck = (Deck.makeStartingDeck).shuffle.unsafeRunSync()
//        .remove(List(overPair.cards.head, overPair.cards(1)), underPair.cards.head, underPair.cards(1))
//
//      val board1: Preflop = Preflop(List(pl1, pl2), List.empty[Card], startDeck)
//      val board2 = board1.
//      val ntrials: Int   = 50000
//      val List(overPairEqu, underPairEqu): List[(Int, Double)] =
//        calculateEquity(board, ntrials)
//
//      overPairEqu._2 > .7 &&
//      underPairEqu._2 < .3
//    }
//  }

}
