package pokerTest

import cats.data.NonEmptySet
import cats.effect.IO
import cats.effect.std.Random
import cats.effect.unsafe.implicits.global
import org.scalacheck.Prop._
import org.scalacheck.Properties
import poker.Hand._
import poker.OrderInstances.handOrder
import poker._
import pokerData.BoardGenerators._
import poker.Street
import poker.Street.{Flop, Preflop}

object PlayerStandingTest extends Properties("Player Standing Tests") {

  implicit val test: Random[IO] = Random.scalaUtilRandom[IO].unsafeRunSync()

  property(
    "Preflop: If the winner is a pair, no more than two players can be winning "
  ) = forAll { preflop: Preflop =>
    val winnerList = PlayerStanding.winnerList(preflop)

    val winningRank =
      winnerList.get.map(_._3).fold(HighCard(Two, List.empty[Card])) {
        case (s: Hand, v: Hand) =>
          if (handOrder.compare(s, v) > 0) s else v
      }
    if ((winningRank.score == 2) && (winnerList.get.size > 2)) false
    else if (winningRank.score > 2) false
    else true
  }

  property(
    "Preflop: If the winner is a HighCard, no more than 4 players can be winning "
  ) = forAll { preflop: Preflop =>
    val winnerList = PlayerStanding.winnerList(preflop)

    val winningRank =
      winnerList.get.map(_._3).fold(HighCard(Two, List.empty[Card])) {
        case (s: Hand, v: Hand) =>
          if (handOrder.compare(s, v) > 0) s else v
      }
    if ((winningRank.score == 1) && (winnerList.get.size > 4)) false
    else if (winningRank.score > 2) false
    else true
  }

  property("No more than one player can have quads on the Flop") = forAll {
    flop: Flop =>
      PlayerStanding(flop).count { case (_, _, hand) =>
        hand match {
          case _: FourOfAKind => true
          case _              => false
        }
      } <= 1
  }

  property("No more than two players can have a StraightFlush on the Flop") =
    forAll { flop: Flop =>
      PlayerStanding(flop).count { case (_, _, hand) =>
        hand match {
          case _: StraightFlush => true
          case _                => false
        }
      } <= 2
    }

  property(
    "No more than two players can have a winning FullHouse on the Flop"
  ) = forAll { flop: Flop =>
    val strFLWinners = for {
      winners <- PlayerStanding.winnerList(flop)
    } yield winners.count { case (_, _, hand) =>
      hand match {
        case _: FullHouse => true
        case _            => false
      }
    } <= 2
    strFLWinners.get // TODO why do I always end up with something unsafe??
  }

  property("No more than one player can have a winning Flush on the Flop") =
    forAll { flop: Flop =>
      val flushWinners = for {
        winners <- PlayerStanding.winnerList(flop)
      } yield winners.count { case (_, _, hand) =>
        hand match {
          case _: Flush => true
          case _        => false
        }
      } <= 1
      flushWinners.get ?= true // TODO why do I always end up with something unsafe??
    }

  property("No more than 5 players can have a Flush on the Flop") = forAll {
    flop: Flop =>
      PlayerStanding(flop).count { case (_, _, hand) =>
        hand match {
          case _: Flush => true
          case _        => false
        }
      } <= 5
  }

  property("No more than 4 players can have a winning Straight on the Flop") =
    forAll { flop: Flop =>
      val straightWinners = for {
        winners <- PlayerStanding.winnerList(flop)
      } yield winners.count { case (_, _, hand) =>
        hand match {
          case _: Straight => true
          case _           => false
        }
      } <= 4
      straightWinners.get ?= true // TODO why do I always end up with something unsafe??
    }

  property(
    "No more than 4 players can have a winning ThreeOfAKind on the Flop"
  ) = forAll { flop: Flop =>
    val setWinners = for {
      winners <- PlayerStanding.winnerList(flop)
    } yield winners.count { case (_, _, hand) =>
      hand match {
        case _: ThreeOfAKind => true
        case _               => false
      }
    } <= 4
    setWinners.get ?= true
  }

  property("No more than 3 players can have a winning TwoPair on the Flop") =
    forAll { flop: Flop =>
      val twoPairWinners =
        PlayerStanding.winnerList(flop).get.count { case (_, _, hand) =>
          hand match {
            case _: TwoPair => true
            case _          => false
          }
        } <= 3

      twoPairWinners ?= true

    }

}
