package EquityTest

import cats.effect.unsafe.implicits.global
import org.scalacheck.Prop.{forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import poker.Deck.StartingDeck
import poker.Street.{Flop, River, Turn}
import poker.{Card, Hand, ShowDown, Street}
import poker.Hand._
import pokerData.BoardGenerators._

object BoardStateTest extends Properties("BoardState Tests") {

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
//    ShowDown(preFlop.allHands).exists(HandRank(_) > Pair) ?= false

    ShowDown(preFlop.allHands).exists(handRank =>
      handRank match {
        case _: Pair | _: HighCard => true
        case _                     => false
      }
    ) ?= true
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

  property("Flop: more than one player can not have quads") = forAll(genFlopBoard) { flop =>
//    ShowDown(flop.allHands).filter(HandRank(_) == FourOfAKind).size <= 1

    ShowDown(flop.allHands).count(p =>
      p match {
        case _: FourOfAKind => true
        case _              => false
      }
    ) <= 1
  }
//
//  property("flop deck size equals 49 minus the players cards and maintains uniqueness") = forAll(genFlopBoard) { flop =>
//    val numPlayers = flop.players.size
//    val playerCards = flop.players.foldLeft(List.empty[Card]) { (s, v) =>
//      v.card1 :: v.card2 :: s
//    }
//    ("Flop Deck size" |: (flop.deck.size ?= 49 - (numPlayers * 2))) &&
//    ("Player card count" |: (playerCards.distinct.size ?= playerCards.size)) &&
//    ("Flop deck still unique" |: (flop.deck.cards.distinct.size ?= flop.deck.size))
//  }
//
//  property("After dealing a flop deck has less cards than a preflop deck") = forAll(genPreflopBoard) { preflop =>
//    val flop: Street = Street.deal(preflop)
//
//    (flop match {
//      case Flop(_) => true
//      case _       => false
//    }) &&
//    (preflop.deck.size > (flop match {
//      case Flop(_, deck, _, _, _) => deck.size
//    }))
//  }
//
//  property("After dealing a turn deck has less cards than a flop deck") = forAll(genFlopBoard) { flop =>
//    val turn: Street = Street.deal(flop)
//    (turn match {
//      case Turn(_) => true
//      case _       => false
//    }) &&
//    (flop.deck.size > (turn match {
//      case Turn(_, deck, _, _, _, _) => deck.size
//    }))
//  }
//
//  property("After dealing a river deck has less cards than a turn deck") = forAll(genTurnBoard) { turn =>
//    val river: Street = Street.deal(turn)
//    (river match {
//      case River(_) => true
//      case _        => false
//    }) &&
//    (turn.deck.size > (river match {
//      case River(_, deck, _, _, _, _, _) => deck.size
//    }))
//  }

}
