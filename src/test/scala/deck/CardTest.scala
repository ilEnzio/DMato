package deck

//import deck.Deck._
//import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CardTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  // Test the name of various Cards
  //
  // ToDO: move this generator; but where??
  // card companion or data generation object?

  val genSuit = Gen.oneOf(Suit.all)
  val genRank = Gen.oneOf(Rank.all)
  val genCard = for {
    rank <- genRank
    suit <- genSuit
  } yield Card(rank, suit)

  val genDeck = Deck.make

  implicit val arbCard = Arbitrary(genCard)
  implicit val arbDeck = Arbitrary(genDeck)
//  println(genCard.sample)
//  println(genCard.sample)
//  println(genCard.sample)

  // Properties of Deck
  // Full deck has length of 52
  // every card is unique
  //

  test("a deck has 52 cards") {
    forAll { (deck: Deck) =>
      assert(deck.size == 52)
    }
  }
}
