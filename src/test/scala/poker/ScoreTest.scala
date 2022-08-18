package poker

import org.scalacheck.Prop.{forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import project.DataGenerators._

object ScoreTest extends Properties("Score Test") {

  property("Pair hand has score < 15,000,000") = forAll(genPair) { hand =>
    (hand.toScore.value < 15000000) ++
      (hand.toScore.value > 1000000)

  }

  property("a High Card hand has a score < 1,000,000") = forAll(genHand) { hand =>
    val rankingList = HandRank.all.filterNot(_ == HighCard)
    (!rankingList.contains(HandRank(hand))) ==> {
      (hand.toScore.value < 1000000)
    }
  }

}
