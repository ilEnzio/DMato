package EquityTest

import cats.effect.unsafe.implicits.global
import org.scalacheck.Prop.{forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import poker.Deck.StartingDeck
import poker.Street.{deal, Flop, River, Turn}
import poker.{Card, Hand, ShowDown, Street}
import poker.Hand._
import poker.OrderInstances.handOrdering
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
//    ShowDown(preFlop.allHands).exists(HandRank(_) > Pair) ?= false

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
  //TODO -  make sure that the same card is never dealt more than once. this is failing!!
  // Cards are being dealt in the flop and also in the players hands
// TODO This is not a ShowDown operation, I'm asking what the current hand street is!!!
  property("Flop: more than one player can not have quads") = forAll(genFlopBoard) { flop =>
//    ShowDown(flop.allHands).filter(HandRank(_) == FourOfAKind).size <= 1
//    println(flop.allHands.sorted.reverse)
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
