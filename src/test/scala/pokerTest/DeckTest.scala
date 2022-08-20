package pokerTest

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalatest.funsuite.AnyFunSuite
import poker.{Card, Deck}
import test.pokerData.DataGenerators._
//import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

object DeckPropTest extends Properties("DeckTest") {

  // Test the name of various Cards

  property("can shuffle a deck") = forAll { (deck: Deck) =>
    val shuffled = deck.shuffle
    (deck.size == shuffled.size &&
    deck.size == shuffled.cards.distinct.size &&
    !deck.cards.sameElements(shuffled.cards))
  }

  property("a starting deck contains only one of any card") = forAll { card: Card =>
    startingDeck.cards.count(c => c == card) == 1 && startingDeck.size == 52
  }

  property("a starting deck contains one of every card") =
    startingDeck.size == 52 && (for (card <- Deck.all) yield startingDeck.cards.count(_ == card) == 1).forall(_ == true)
}
