package EquityTest

import org.scalacheck.Prop.{forAll, propBoolean, AnyOperators, False}
import org.scalacheck.Properties
import poker.BoardState.{Flop, Preflop, River, Turn}
import poker.{BoardState, Card, Hand, ShowDown}
import poker.Hand._
import pokerData.DataGenerators._

object BoardStateTest extends Properties("BoardState Tests") {

  // genFlop, genTurn, genRiver
  // genPlayer,

  property("Preflop hands can never be ranked higher than a Pair") = forAll(genPreflop) { preFlop =>
    preFlop.players.exists(p =>
      Hand.rank(List(p.card1, p.card2)) match {
        case _: Pair | _: HighCard => true
        case _                     => false
      }
    ) ?= true
  }

  property("The winning hand Preflop is a pair or less") = forAll(genPreflop) { preFlop =>
//    ShowDown(preFlop.allHands).exists(HandRank(_) > Pair) ?= false

    ShowDown(preFlop.allHands).exists(handRank =>
      handRank match {
        case _: Pair | _: HighCard => true
        case _                     => false
      }
    ) ?= true
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
//    ShowDown(flop.allHands).filter(HandRank(_) == FourOfAKind).size <= 1

    ShowDown(flop.allHands).count(p =>
      p match {
        case _: FourOfAKind => true
        case _              => false
      }
    ) <= 1
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

  property("After dealing a flop deck has less cards than a preflop deck") = forAll(genPreflop) { preflop =>
    val flop: BoardState = BoardState.deal(preflop)

    (flop match {
      case Flop(_) => true
      case _       => false
    }) &&
    (preflop.deck.size > (flop match {
      case Flop(_, deck, _, _, _) => deck.size
    }))
  }

  property("After dealing a turn deck has less cards than a flop deck") = forAll(genFlop) { flop =>
    val turn: BoardState = BoardState.deal(flop)
    (turn match {
      case Turn(_) => true
      case _       => false
    }) &&
    (flop.deck.size > (turn match {
      case Turn(_, deck, _, _, _, _) => deck.size
    }))
  }

  property("After dealing a river deck has less cards than a turn deck") = forAll(genTurn) { turn =>
    val river: BoardState = BoardState.deal(turn)
    (river match {
      case River(_) => true
      case _        => false
    }) &&
    (turn.deck.size > (river match {
      case River(_, deck, _, _, _, _, _) => deck.size
    }))
  }

}
