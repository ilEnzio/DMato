package EquityTest

import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxPartialOrder
import org.scalacheck.Prop.{forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import poker.Deck.StartingDeck
import poker.Street.{deal, Flop, River, Turn}
import poker.{Card, Hand, PlayerStanding, ShowDown, Street, Two}
import poker.Hand._
import poker.OrderInstances.{handOrder, handOrdering}
import pokerData.BoardGenerators._

object StreetTest extends Properties("BoardState Tests") {

  // genFlop, genTurn, genRiver
  // genPlayer,

  property("Preflop hands can never be ranked higher than a Pair") = forAll(genPreflopBoard) { preFlop =>
    preFlop.players.exists(p =>
      Hand.rank(List(p.card1, p.card2)) match {
        case _: Pair | _: HighCard => true
        case _                     => false
      }
    ) ?= true
  }

  property("The winning hand Preflop is a pair or less") = forAll(genPreflopBoard) { preFlop =>
    ShowDown(preFlop.allHands).exists(handRank =>
      handRank match {
        case _: Pair | _: HighCard => true
        case _                     => false
      }
    ) ?= true
  }

  property("No card is ever dealt more than once through to the River") = forAll(genPreflopBoard) { preflop =>
    val flop          = deal(preflop)
    val turn          = deal(flop)
    val river         = deal(turn).asInstanceOf[River] // TODo Does this mean that I need to change the Street trait/contract
    val riverCards    = List(river.card1, river.card2, river.card3, river.turn, river.river)
    val holeCards     = preflop.players.collect(p => List(p.card1, p.card2)).flatten
    val allDealtCards = holeCards :: riverCards
    allDealtCards.distinct ?= allDealtCards

  }
  property("Preflop: If the winner is a pair, no more than two players can be winning ") = forAll(genPreflopBoard) {
    preFlop =>
      val winnerList = PlayerStanding.winnerList(preFlop)

      val winningRank = winnerList.get.map(_._3).fold(HighCard(Two, List.empty[Card])) { case (s: Hand, v: Hand) =>
        if (handOrder.compare(s, v) > 0) s else v
      }
      if ((winningRank == Pair) && (winnerList.get.size > 2)) false
      else if (winningRank.score > 2) false
      else true
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

// TODO This is not a ShowDown operation, I'm asking what the current hand street is!!!
  property("Flop: more than one player can NOT have quads") = forAll(genFlopBoard) { flop =>
    ShowDown(flop.allHands).count(p =>
      p match {
        case _: FourOfAKind => true
        case _              => false
      }
    ) <= 1
  }

}
