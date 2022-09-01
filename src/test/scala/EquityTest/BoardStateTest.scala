package EquityTest

import cats.implicits.catsSyntaxPartialOrder
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties
import poker.BoardState.Preflop
import poker.{Hand, HandRank, Pair, ShowDown}
import poker.OrderInstances.{handRankingOrder}
import test.pokerData.DataGenerators._

object BoardStateTest extends Properties("BoardState Tests") {

  // genPreFlop, genFlop, genTurn, genRiver
  // genPlayer,

  property("Preflop hands can never be ranked higher than a Pair") = forAll(genPreflop) { preFlop =>
    preFlop.players.filter(p => HandRank(Hand(List(p.card1, p.card2))) > Pair) == Nil
  }

  property("The winning hand preflop is a pair or less") = forAll(genPreflop) { preFlop =>
    ShowDown(preFlop.allHands).filter(HandRank(_) > Pair) == Nil
  }

}
