package pokerTest

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import poker.Hand.{FourOfAKind, HighCard}
import poker.OrderInstances.handOrder
import poker.{Card, Hand, PlayerStanding, Two}
import pokerData.BoardGenerators.{genFlopBoard, genPreflopBoard}

object PlayerStandingTest extends Properties("Player Standing Tests") {

  property("Preflop: If the winner is a pair, no more than two players can be winning ") = forAll(genPreflopBoard) {
    preFlop =>
      val winnerList = PlayerStanding.winnerList(preFlop)

      val winningRank = winnerList.get.map(_._3).fold(HighCard(Two, List.empty[Card])) { case (s: Hand, v: Hand) =>
        if (handOrder.compare(s, v) > 0) s else v
      }
      if ((winningRank.score == 2) && (winnerList.get.size > 2)) false
      else if (winningRank.score > 2) false
      else true
  }

  property("Preflop: If the winner is a HighCard, no more than 4 players can be winning ") = forAll(genPreflopBoard) {
    preFlop =>
      val winnerList = PlayerStanding.winnerList(preFlop)

      val winningRank = winnerList.get.map(_._3).fold(HighCard(Two, List.empty[Card])) { case (s: Hand, v: Hand) =>
        if (handOrder.compare(s, v) > 0) s else v
      }
      if ((winningRank.score == 1) && (winnerList.get.size > 4)) false
      else if (winningRank.score > 2) false
      else true
  }

  property("No more than one player can have quads on the Flop") = forAll(genFlopBoard) { flop =>
    val quadWinners = for {
      winners <- PlayerStanding.winnerList(flop)
    } yield winners.count { case (_, _, hand) =>
      hand match {
        case _: FourOfAKind => true
        case _              => false
      }
    } <= 1

    quadWinners.get
  }

}
