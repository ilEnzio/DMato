package pokerTest

import cats.effect.unsafe.implicits.global
import org.scalacheck.Gen.pick
import org.scalacheck.Prop.{forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import poker.Deck.StartingDeck
import poker.OrderInstances.rankOrder
import poker.{Card, Deck, Suit}
import pokerData.DeckGenerators._

object DeckPropTest extends Properties("DeckTest") {

  property("a starting deck contains one of every card") =
    StartingDeck.all.size == 52 && (for (card <- StartingDeck.all)
      yield StartingDeck.all.count(_ == card) == 1).forall(_ == true)

}
