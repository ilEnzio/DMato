package pokerTest

import cats.effect.unsafe.implicits.global
import org.scalacheck.Gen.{const, pick}
import org.scalacheck.Prop.{forAll, propBoolean, AnyOperators}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalatest.funsuite.AnyFunSuite
import poker.OrderInstances.{cardOrder, pairOrder, rankOrder}
import poker.{BoardState, Card, Deck, Suit}
import test.pokerData.DataGenerators._
//import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

object DeckPropTest extends Properties("DeckTest") {

  // Test the name of various Cards
  //TODO - unsafeRunSync????
  property("can shuffle a deck") = forAll { (deck: Deck) =>
    val shuffled = deck.shuffle.unsafeRunSync()
    (deck.size == shuffled.size &&
    deck.size == shuffled.cards.distinct.size &&
    !deck.cards.sameElements(shuffled.cards))
  }

  property("a starting deck contains only one of any card") = forAll { card: Card =>
    startingDeck.cards.count(c => c == card) == 1 && startingDeck.size == 52
  }

  property("a starting deck contains one of every card") =
    startingDeck.size == 52 && (for (card <- Deck.all) yield startingDeck.cards.count(_ == card) == 1).forall(_ == true)

  property("remove / add roundtrip  single card") = forAll(genCard) { card =>
    val deck = startingDeck.shuffle.unsafeRunSync()
    (deck.remove(card).size ?= 51) :| "Remove card" &&
    (deck.size ?= deck
      .remove(card)
      .add(card)
      .size) :| "Round trip remove/add"

  }

  property("remove / add roundtrip  multiple cards") = forAll(genRank, genRank) { (overRank, underRank) =>
    (rankOrder.compare(overRank, underRank) > 0) ==> {
      val pl1 = pick(2, Suit.all).sample.get.map(Card(overRank, _)).toList
      val pl2 = pick(2, Suit.all).sample.get.map(Card(underRank, _)).toList

      val playerCards = pl1 ++ pl2
      val deck        = startingDeck
      "Round trip remove/add" |: (deck.size ?= deck
        .remove(playerCards)
        .add(playerCards)
        .size)

    }
  }

}
