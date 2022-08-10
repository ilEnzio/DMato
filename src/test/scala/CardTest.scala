import deck._
import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.Prop
import org.scalacheck.Gen



class CardTest extends AnyFunSuite {

  // Test the name of various Cards
  //

  val genSuit = Gen.oneOf(Suit.all)
  val genRank = Gen.oneOf(Rank.all)
  val genCard = for {
    rank <- genRank
    suit <- genSuit
  } yield Card(rank, suit)

  println(genCard.sample)
  println(genCard.sample)
  println(genCard.sample)

}
