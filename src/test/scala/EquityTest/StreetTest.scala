package EquityTest

import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxPartialOrder
import org.scalacheck.Prop.{forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import poker.Deck.StartingDeck
import poker.Street._
import poker._
import poker.Hand._
import poker.OrderInstances.{handOrder, handOrdering}
import pokerData.BoardGenerators._

object StreetTest extends Properties("Street Tests") {

  // genFlop, genTurn, genRiver
  // genPlayer,

  property("Preflop hands can never be ranked higher than a Pair") = forAll { (preflop: Street.Preflop) =>
    preflop.players.exists(p =>
      Hand.rank(List(p.card1, p.card2)) match {
        case _: Pair | _: HighCard => true
        case _                     => false
      }
    ) ?= true
  }

  property("The winning hand Preflop is a pair or less") = forAll { (preflop: Street.Preflop) =>
    ShowDown(preflop.allHands).exists(handRank =>
      handRank match {
        case _: Pair | _: HighCard => true
        case _                     => false
      }
    ) ?= true
  }

  property("No card is ever dealt more than once through to the River") = forAll { (preflop: Street.Preflop) =>
    val river         = preflop.dealTillRiver // TODO Does this mean that I need to change the Street trait/contract
    val riverCards    = List(river.card1, river.card2, river.card3, river.turn, river.river)
    val holeCards     = preflop.players.collect(p => List(p.card1, p.card2)).flatten
    val allDealtCards = holeCards :: riverCards
    allDealtCards.distinct ?= allDealtCards

  }

  //  property("preflop deck size equals 52 minus the players cards and maintains uniqueness") = forAll(genPreflopBoard, genNumberOfPlayers) {
//    (preFlopBoard, numPlayers) =>
//      val numPlayers = preFlopBoard.players.size
//      val playerCards = preFlopBoard.players.foldLeft(List.empty[Card]) { (s, v) =>
//        v.card1 :: v.card2 :: s
//      }
// TODO how do I test without violating encapsulation?
//      preFlopBoard.deck. == 52 - (numPlayers * 2) &&
//      playerCards.distinct.size == playerCards.size &&
//      preFlopBoard.deck.cards.distinct.size == preFlopBoard.deck.size
//  }

}
