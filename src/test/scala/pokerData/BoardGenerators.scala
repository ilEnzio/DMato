package pokerData

import cats.effect.unsafe.implicits.global
import org.scalacheck.Gen.{choose, oneOf}
import org.scalacheck._
import poker.Deck.startingDeck
import poker.Street._
import poker._

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

  def genPreflopBoard(numPlayers: Int): Gen[Preflop] =
    startingDeck.dealHoleCards(numPlayers).unsafeRunSync()

  implicit val arbPreflop: Arbitrary[Preflop] =
    Arbitrary(genNumberOfPlayers.flatMap(genPreflopBoard))

  def genFlopBoard(numPlayers: Int): Gen[Flop] =
    for {
      preflop <- genPreflopBoard(numPlayers)
    } yield dealFlop(preflop)

  implicit val arbFlop: Arbitrary[Flop] =
    Arbitrary(genNumberOfPlayers.flatMap(genFlopBoard))

  val genTurnBoard: Gen[Turn] = {
    for {
      numPlayers <- genNumberOfPlayers
      flop       <- genFlopBoard(numPlayers)
    } yield dealTurn(flop)

  }

  val genRiverBoard: Gen[River] = {
    for {
      turn <- genTurnBoard
    } yield dealRiver(turn)
  }

}
