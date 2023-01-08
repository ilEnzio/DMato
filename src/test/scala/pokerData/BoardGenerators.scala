package pokerData

import cats.effect.IO
import cats.effect.std.Random
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.{choose, oneOf}
import poker.Deck.startingDeck
import poker.Street.{dealFlop, dealRiver, dealTurn, Flop, Preflop, River, Turn}
import poker.{
  BigBlind,
  Button,
  CutOff,
  HighJack,
  Position,
  SmallBlind,
  UTG,
  UTGP1,
  UTGP2,
  UTGP3
}

object BoardGenerators {

  //TODO create Ordering for Position

  val genPosition: Gen[Position] = oneOf(
    SmallBlind,
    BigBlind,
    UTG,
    UTGP1,
    UTGP2,
    UTGP3,
    HighJack,
    CutOff,
    Button
  )

  implicit val arbPosition: Arbitrary[Position] = Arbitrary(genPosition)

  val genNumberOfPlayers: Gen[Int] =
    choose(2, 10)

  def genPreFlopBoard(
    numPlayers: Int
  ): Gen[Preflop] =
//    implicit val ioRandom: Random[IO] =
//      Random.scalaUtilRandom[IO].unsafeRunSync()
//    Gen.const(startingDeck.dealHoleCards[IO](numPlayers).unsafeRunSync())
    Gen.delay {
      implicit val ioRandom: Random[IO] =
        Random.scalaUtilRandom[IO].unsafeRunSync()
      startingDeck.dealHoleCards[IO](numPlayers).unsafeRunSync()
    }

  implicit val arbPreflop: Arbitrary[Preflop] =
    Arbitrary(genNumberOfPlayers.flatMap(genPreFlopBoard))

  def genFlopBoard(
    numPlayers: Int
  ): Gen[Flop] =
    for {
      preflop <- genPreFlopBoard(numPlayers)
    } yield dealFlop(preflop)

  implicit val arbFlop: Arbitrary[Flop] =
    Arbitrary(genNumberOfPlayers.flatMap(genFlopBoard))

  def genTurnBoard: Gen[Turn] =
    for {
      numPlayers <- genNumberOfPlayers
      flop       <- genFlopBoard(numPlayers)
    } yield dealTurn(flop)

  def genRiverBoard: Gen[River] =
    for {
      turn <- genTurnBoard
    } yield dealRiver(turn)

}
