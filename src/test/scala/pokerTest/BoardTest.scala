package pokerTest

import org.scalacheck.Prop.{all, forAll, propBoolean, AnyOperators}
import org.scalacheck.{Arbitrary, Gen, Properties}
import poker.{Flush, FourOfAKind, Hand, HighCard, Pair, Straight, StraightFlush, ThreeOfAKind, TwoPair}
import test.pokerData.DataGenerators._
import poker._

object BoardTest extends Properties("Board Tests") {
//
//  property("Preflop player hands are at most 1 pair") = { (deck: startingDeck) => }

}
