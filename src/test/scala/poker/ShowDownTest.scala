package poker

import org.scalacheck.Prop.{forAll, propBoolean, AnyOperators}
import org.scalacheck.Properties
import cats.implicits.catsSyntaxPartialOrder
import poker.OrderInstances._
import project.DataGenerators.{genAceHigh, genHand, genHighCard, genStraightFlush}

object ShowDownTest extends Properties("ShowDownTest") {

  property("StraightFlush is greater than any other hand Ranking") = forAll(genStraightFlush, genHand) {
    (strFlush, other) =>
      HandRank(other) != StraightFlush ==> {
        HandRank(strFlush) > HandRank(other)
      }
  }

  property("HighCard - Ace high is greater than any other high card") = forAll(genAceHigh, genHighCard) {
    (aHigh, other) =>
      (other.cards.sorted.reverse(0).rank != Ace) ==> {
        "Ace vs Other" |: (ShowDown(List(aHigh, other)) ?= List(aHigh, other))
      }
  }
// TODo this is failing because my AceGen (and HighCard gen!) are not varied enough.
  property("HighCard - Two Ace High but second rank wins") = forAll(genAceHigh, genAceHigh) { (fst, snd) =>
    if (fst.cards.sorted.reverse(1) > snd.cards.sorted.reverse(1))
      (ShowDown(List(fst, snd)) ?= List(fst, snd))
    else
      (ShowDown(List(fst, snd)) ?= List(snd, fst))
  }
}
