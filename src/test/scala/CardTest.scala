import deck._
import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.Prop
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

class CardTest extends AnyFunSuite {

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

  implicit val arbCard = Arbitrary(genCard)

  println(genCard.sample)
  println(genCard.sample)
  println(genCard.sample)

  // Properties of Deck
  // Full deck has length of 52
  // every card is unique
  //

}
