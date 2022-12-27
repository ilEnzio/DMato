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

  def genPreflopBoard(numPlayers: Int): Gen[Preflop] =
    startingDeck.dealHoleCards(numPlayers).unsafeRunSync()

  val boardState: Flop = dealFlop(genPreflopBoard(2).sample.get)

  val currBoard: mutable.Map[Int, Street] = mutable.Map(1 -> boardState)

  final case class FlopCards(cards: List[Card])

  def boardRoutes[F[_]: Monad]: HttpRoutes[F] = {

    val dsl = Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] { case GET -> Root / "flop" =>
      currBoard.get(1) match {
        case Some(street) =>
          street match {
            case _: Preflop =>
              BadRequest("There is no flop on a Preflop Board")
            case x: Flop =>
              Ok(FlopCards(List(x.card1, x.card2, x.card3)).asJson)
            case x: Turn =>
              Ok(FlopCards(List(x.card1, x.card2, x.card3)).asJson)
            case x: River =>
              Ok(FlopCards(List(x.card1, x.card2, x.card3)).asJson)
          }
        case None => NotFound("No cards have been dealt??")
      }
    //      case GET -> Root / "turn" =>
    //        ???
    //
    //      case GET -> Root / "river" =>
    //        ???
    //    }
    }
  }

  def playersRoutes[F[_]: Monad]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] { case GET -> Root / "players" =>
      currBoard.get(1) match {
        case Some(street) =>
          street match {
            case _: Street => Ok(street.players.asJson)
          }
        case None => NotFound("No cards have been dealt??")
      }
    }
  }

  def allRoutes[F[_]: Concurrent]: HttpRoutes[F] =
    boardRoutes[F] <+> playersRoutes[F]

  def allRoutesComplete[F[_]: Concurrent]: HttpApp[F] = allRoutes.orNotFound

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
