package pokerData

import cats._
import cats.effect.IO
import cats.effect.std.Random
import cats.implicits._
import cats.effect.unsafe.implicits.global
import org.scalacheck.Gen.{choose, oneOf}
import org.scalacheck._
import poker.Deck.{startingDeck, StartingDeck, StartingDeckImpl}
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

  def genPreflopBoard[F[_]: Functor: Random](numPlayers: Int): Gen[Preflop] =
    startingDeck.dealHoleCards[F](numPlayers).unsafeRunSync()

  def arbPreflop[F[_]: Functor: Random]: Gen[Preflop] =
    genNumberOfPlayers.flatMap(genPreflopBoard[F])

  def genFlopBoard[F[_]: Functor: Random](numPlayers: Int): Gen[Flop] =
    for {
      preflop <- genPreflopBoard[F](numPlayers)
    } yield dealFlop(preflop)

  def arbFlop[F[_]: Functor: Random]: Gen[Flop] =
    genNumberOfPlayers.flatMap(genFlopBoard[F])

  def genTurnBoard[F[_]: Functor: Random]: Gen[Turn] =
    for {
      numPlayers <- genNumberOfPlayers
      flop       <- genFlopBoard[F](numPlayers)
    } yield dealTurn(flop)

  def genRiverBoard[F[_]: Functor: Random]: Gen[River] =
    for {
      turn <- genTurnBoard[F]
    } yield dealRiver(turn)

}
