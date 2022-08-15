package poker

import org.scalacheck.Prop.{forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import poker.RankingTest.genHand

object ScoreTest extends Properties("Score Test") {

  //TODO
  property("a High Card hand has a score < 1,000,000") = forAll(genHand) { hand =>
    val rankingList = Ranking.all.filterNot(_ == HighCard)
    (!rankingList.contains(Ranking(hand))) ==> {
      (hand.toScore.value < 1000000)
    }
  }

}
