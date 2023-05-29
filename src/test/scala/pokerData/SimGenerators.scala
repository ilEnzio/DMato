package pokerData

import equity._
import org.scalacheck.Gen.{choose, oneOf}
import org.scalacheck.{Arbitrary, Gen}
import poker._
import pokerData.BoardGenerators.genNumberOfPlayers

object SimGenerators {

  val genPosition: Gen[Position] = oneOf(
    SmallBlind,
    BigBlind,
    UTG,
    UTGP1,
    UTGP2,
    UTGP3,
    LoJack,
    HighJack,
    CutOff,
    Button
  )

  implicit val arbPosition: Arbitrary[Position] = Arbitrary(genPosition)

  val genNumberOfBoardCards: Gen[Int] =
    choose(0, 5)

  def genSimSetup(numPlayers: Int): Gen[SimSetup[Option]] =
    // Generate a number of cards from 0 to the number of players times 2
    // Generate the SimPlayers from that.
    // Generate a number of cards from 0 - 5; Generate the board from that.
    //
    for {
      numPlayerCards <- genNumberOfPlayers
      numBoardCards  <- genNumberOfBoardCards

      /// I guess I could use the State monad... again??

    } yield ???

}
