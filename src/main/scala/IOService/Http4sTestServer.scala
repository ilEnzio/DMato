package IOService

import cats.effect._
import cats.effect.{Concurrent, ExitCode, IO, IOApp}
import cats.syntax.all._
import cats.Monad
import io.circe.generic.auto._
import io.circe.syntax._
import _root_.io.circe.Encoder
import cats.effect.std.Random
import cats.effect.unsafe.implicits.global
import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.circe._
import org.http4s.server.Router
import org.scalacheck.Gen
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

  implicit val ioRandom: Random[IO] =
    Random.scalaUtilRandom[IO].unsafeRunSync()

  def boardState: IO[Preflop] = startingDeck.dealHoleCards[IO](5)

  val ioFlop: IO[Flop] = for {
    x <- boardState
  } yield dealFlop(x)

  case class FlopCards(card1: Card, card2: Card, card3: Card)

  implicit val rankEncoder: Encoder[Rank] =
    Encoder[String].contramap((rank: Rank) => rank.toString)

  implicit val suitEncoder: Encoder[Suit] =
    Encoder[String].contramap((suit: Suit) => suit.toString)

  def boardRoutes[F[_]: Monad: LiftIO]: HttpRoutes[F] = {

    val dsl = Http4sDsl[F]
    import dsl._
    HttpRoutes.of[F] { case GET -> Root / "flop" =>
      LiftIO[F].liftIO(ioFlop).flatMap { flop =>
        Ok(
          FlopCards(flop.card1, flop.card2, flop.card3).asJson.noSpaces
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
