package equity

import cats._
import cats.syntax.all._
import cats.effect._
//
//object TempExample {
//
//  sealed trait Face
//  case object _1 extends Face
//  case object _2 extends Face
//  case object _3 extends Face
//  case object _4 extends Face
//  case object _5 extends Face
//  case object _6 extends Face
//
//  final case class ThreeDiceIn[F[_]](
//    die1: F[Face],
//    die2: F[Face],
//    die3: F[Face]
//  ) {
//    def mapK[G[_]](f: F[Face] => G[Face]): ThreeDiceIn[G] =
//      ThreeDiceIn(
//        f(die1),
//        f(die2),
//        f(die3)
//      )
//
//    def sequenceK(implicit F: Applicative[F]): F[ThreeDiceIn[Id]] =
//      (die1, die2, die3).mapN(ThreeDiceIn.apply)
//  }
//
//  def applyK[A](
//    x: ThreeDiceIn[Id],
//    f: ThreeDiceIn[Function1[*, A]]
//  ): ThreeDiceIn[Function1[*, A]] = ???
//
//  val example: ThreeDiceIn[List] =
//    ThreeDiceIn(
//      die1 = List(_2, _3),
//      die2 = List(_4, _3),
//      die3 = List(_4, _5, _6)
//    )
//
//  val example2: ThreeDiceIn[Function1[*, Int]] =
//    ThreeDiceIn(
//      die1 = (face: Face) => 7,
//      die2 = (face: Face) => 9,
//      die3 = (face: Face) => 11
//    )
//
//  val example3: ThreeDiceIn[Option] =
//    ThreeDiceIn(
//      die1 = None,
//      die2 = Some(_4),
//      die3 = None
//    )
//
//  type UpToThreeDice = ThreeDiceIn[Option]
//  type ThreeDice     = ThreeDiceIn[Id]
//
//  def roll: IO[ThreeDice] =
//    ThreeDiceIn[Id](_2, _4, _6).pure[IO]
//
//  def getOrRoll(maybe: Option[Face]): IO[Face] =
//    maybe.fold(_3.pure[IO])(_.pure[IO])
//
//  def fill(x: UpToThreeDice): IO[ThreeDice] =
//    x.mapK(getOrRoll).sequenceK
//
//  def score(guess: UpToThreeDice, rolled: ThreeDice): Int =
//    0
//
//}
