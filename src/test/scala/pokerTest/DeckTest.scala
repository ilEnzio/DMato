package pokerTest

import cats.effect.unsafe.implicits.global
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalatest.funsuite.AnyFunSuite
import poker.{BoardState, Card, Deck}
import test.pokerData.DataGenerators._
//import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

object DeckPropTest extends Properties("DeckTest") {

  // Test the name of various Cards
//TODO - unsafeRunSync????
  property("can shuffle a deck") = forAll { (deck: Deck) =>
    val shuffled = BoardState.shuffleDeck(deck)
    (deck.size == shuffled.unsafeRunSync().size &&
    deck.size == shuffled.unsafeRunSync().cards.distinct.size &&
    !deck.cards.sameElements(shuffled.unsafeRunSync().cards))
  }

  property("a starting deck contains only one of any card") = forAll { card: Card =>
    startingDeck.cards.count(c => c == card) == 1 && startingDeck.size == 52
  }

  property("a starting deck contains one of every card") =
    startingDeck.size == 52 && (for (card <- Deck.all) yield startingDeck.cards.count(_ == card) == 1).forall(_ == true)
}
