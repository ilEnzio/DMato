package EquityTest

import cats.implicits.catsSyntaxPartialOrder
import org.scalacheck.Prop.{forAll, propBoolean, AnyOperators, False}
import org.scalacheck.Properties
import poker.BoardState.Preflop
import poker.{FourOfAKind, Hand, HandRank, Pair, ShowDown}
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

//  property("Test preflop deck size and uniqueness") = ???

  property("Flop: more than one player can not have quads") = forAll(genFlop) { flop =>
    ShowDown(flop.allHands).filter(HandRank(_) == FourOfAKind).size <= 1
  }

}
