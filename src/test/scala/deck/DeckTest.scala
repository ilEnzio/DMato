package deck

//import deck.Deck._
//import org.scalacheck.Prop._
//import deck.CardTestProps.property
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

object DeckPropTest extends Properties("CardTest") {

  // Test the name of various Cards
  // ToDO: move this generator; but where??
  // card companion or data generation object?

  val genSuit = Gen.oneOf(Suit.all)
  val genRank = Gen.oneOf(Rank.all)
  val genCard = for {
    rank <- genRank
    suit <- genSuit
  } yield Card(rank, suit)

  val startingDeck = Deck.makeStartingDeck //

  implicit val arbCard = Arbitrary(genCard)
  implicit val arbDeck = Arbitrary(startingDeck)
//  println(genCard.sample)
//  println(genCard.sample)
//  println(genCard.sample)

  // Properties of Deck
  // Full deck has length of 52
  // every card is unique
  //

  property("can shuffle a full deck") = forAll { (deck: Deck) =>
    val shuffled = Deck.shuffle(deck)
    (deck.size == shuffled.size &&
    deck.size == shuffled.cards.toSet.size &&
    deck.cards != shuffled.cards)
  }

  property("a starting deck contains one of each card") = forAll(genCard) { card =>
    startingDeck.cards.count(c => c == card) == 1 && startingDeck.size == 52
  }

}
