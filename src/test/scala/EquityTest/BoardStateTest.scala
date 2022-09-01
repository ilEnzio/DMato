package EquityTest

import cats.implicits.catsSyntaxPartialOrder
import org.scalacheck.Prop.{forAll, propBoolean, AnyOperators, False}
import org.scalacheck.Properties
import poker.BoardState.Preflop
import poker.{Card, FourOfAKind, Hand, HandRank, Pair, ShowDown}
import poker.OrderInstances.handRankingOrder
import test.pokerData.DataGenerators._

object BoardStateTest extends Properties("BoardState Tests") {

  // genFlop, genTurn, genRiver
  // genPlayer,

  property("Preflop hands can never be ranked higher than a Pair") = forAll(genPreflop) { preFlop =>
    preFlop.players.exists(p => HandRank(Hand(List(p.card1, p.card2))) > Pair) ?= false
  }

  property("The winning hand Preflop is a pair or less") = forAll(genPreflop) { preFlop =>
    ShowDown(preFlop.allHands).exists(HandRank(_) > Pair) ?= false
  }

  property("preflop deck size equals 52 minus the players cards and maintains uniqueness") = forAll(genPreflop) {
    preFlop =>
      val numPlayers = preFlop.players.size
      val playerCards = preFlop.players.foldLeft(List.empty[Card]) { (s, v) =>
        v.card1 :: v.card2 :: s
      }
      preFlop.deck.size == 52 - (numPlayers * 2) &&
      playerCards.distinct.size == playerCards.size &&
      preFlop.deck.cards.distinct.size == preFlop.deck.size
  }

  property("Flop: more than one player can not have quads") = forAll(genFlop) { flop =>
    ShowDown(flop.allHands).filter(HandRank(_) == FourOfAKind).size <= 1
  }

  property("flop deck size equals 49 minus the players cards and maintains uniqueness") = forAll(genFlop) { flop =>
    val numPlayers = flop.players.size
    val playerCards = flop.players.foldLeft(List.empty[Card]) { (s, v) =>
      v.card1 :: v.card2 :: s
    }
    ("Flop Deck size" |: (flop.deck.size ?= 49 - (numPlayers * 2))) &&
    ("Player card count" |: (playerCards.distinct.size ?= playerCards.size)) &&
    ("Flop deck still unique" |: (flop.deck.cards.distinct.size ?= flop.deck.size))
  }

  property()
}
