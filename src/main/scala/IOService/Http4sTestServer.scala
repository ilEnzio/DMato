package IOService
import cats._
import cats.effect._
import cats.implicits._
import org.http4s.circe._
import org.http4s._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.dsl._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import cats.Monad
import cats.effect.unsafe.implicits.global
import cats.effect.{Concurrent, ExitCode, IO, IOApp}
import org.http4s.server.Router
import org.scalacheck.Gen
import poker.Deck.startingDeck
import poker.{Card, Street}
import poker.Street._

import scala.collection.mutable

object Http4sTestServer extends IOApp {

  /*
  - Get all the players and cards of current Street - the board state
  - Post - deal the hole cards -  preflop state
   */

  // Request -> F[Option[Response]]
  // HttpRoutes[F] ^^^

  // Get /flop
  // GET /turn
  // GET /river
  // GET /players

  // ??? Need a DB/state

  def genPreFlopBoard(numPlayers: Int): IO[Preflop] =
    startingDeck.dealHoleCards(numPlayers)

  def boardState: IO[Flop] = genPreFlopBoard(2).map(dealFlop)

  def boardStateErr: IO[Preflop] = genPreFlopBoard(2)

//  def currBoard: mutable.Map[Int, IO[Street]] =
//    mutable.Map(1 -> boardStateErr)

  final case class FlopCards(cards: List[Card])

  def boardRoutes[F[_]: Monad: LiftIO]: HttpRoutes[F] = {

    val dsl = Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] { case GET -> Root / "flop" =>
      LiftIO[F].liftIO(boardState).flatMap { street =>
        Ok(FlopCards(List(street.card1, street.card2, street.card3)).asJson)
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
