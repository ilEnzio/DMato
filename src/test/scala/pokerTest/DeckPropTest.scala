package pokerTest

import org.scalacheck.Prop.{forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import poker.{Deck}

object DeckPropTest extends Properties("DeckTest") {

  property("a starting deck contains one of every card") =
    Deck.all.size == 52 && (for (card <- Deck.all)
      yield Deck.all.count(_ == card) == 1).forall(_ == true)

}
