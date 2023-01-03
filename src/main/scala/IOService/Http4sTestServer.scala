package IOService

import cats.effect._
import cats.syntax.all._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl._
import org.http4s.server.blaze.BlazeServerBuilder
import cats.Monad
import cats.effect.{Concurrent, ExitCode, IO, IOApp}
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.server.Router
import poker.Deck.startingDeck
import poker.{Card, Rank, Street, Suit}
import poker.Street._

object Http4sTestServer extends IOApp {

  /*
  - Get all the players and cards of current Street - the board state
  - Post - deal the hole cards -  preflop state
   */

  // Request => F[Option[Response]]
  // HttpRoutes[F] ^^^

  // Get /flop
  // GET /turn
  // GET /river
  // GET /players

  // ??? Need a DB/state

  def genPreFlopBoard(numPlayers: Int): IO[Preflop] =
    startingDeck.dealHoleCards(numPlayers)

  def boardState: IO[Flop] = genPreFlopBoard(5).map(dealFlop)

  def boardStateErr: IO[Preflop] = genPreFlopBoard(2)

  case class FlopCards(value1: Int, value2: Int, value3: Int)
//
//  implicit val FlopEncoder: Encoder[FlopCards] =
//    Encoder.instance {flop: FlopCards => json"""{"card1": ${flop.card1} }"""

  final case class FlopCardsFlat(
    c1Rank: Rank,
    c1Suit: Suit,
    c2Rank: Rank,
    c2Suit: Suit,
    c3Rank: Rank,
    c3Suit: Suit
  )

  def boardRoutes[F[_]: Monad: LiftIO]: HttpRoutes[F] = {

    val dsl = Http4sDsl[F]
    import dsl._
// TODO somehow I can't deliver a proper/ non-nested json
    HttpRoutes.of[F] {
      case GET -> Root / "flop" =>
        LiftIO[F].liftIO(boardState).flatMap { street =>
          println(street.card1.rank.value.asJson.noSpaces)
          Ok(
            FlopCards(
              street.card1.rank.value,
              street.card2.rank.value,
              street.card3.rank.value
            ).asJson
          )
        }
      case GET -> Root / "flop2" =>
        LiftIO[F].liftIO(boardState).flatMap { s =>
          Ok(
            FlopCardsFlat(
              s.card1.rank,
              s.card1.suit,
              s.card2.rank,
              s.card2.suit,
              s.card3.rank,
              s.card3.suit
            ).asJson
          )
        }
    }
  }

  def playersRoutes[F[_]: Monad: LiftIO]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] { case GET -> Root / "players" =>
      LiftIO[F].liftIO(boardState).flatMap { street =>
        Ok(street.players.asJson)
      }
    }
  }

  def allRoutes[F[_]: Concurrent: LiftIO]: HttpRoutes[F] =
    boardRoutes[F] <+> playersRoutes[F]

  // not sure how to use this
  def allRoutesComplete[F[_]: Concurrent: LiftIO]: HttpApp[F] =
    allRoutes.orNotFound

  import scala.concurrent.ExecutionContext.global

  override def run(args: List[String]): IO[ExitCode] = {

    val apis = Router(
      "/api" -> Http4sTestServer.boardRoutes[IO],
      "/api" -> Http4sTestServer.playersRoutes[IO]
    ).orNotFound

    BlazeServerBuilder[IO](global)
      .bindHttp(8080, "localhost")
      .withHttpApp(apis)
      .resource
      .use(_ => IO.never)
      .as(ExitCode.Success)

  }

}
