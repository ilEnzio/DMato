package poker

import cats.Functor
import cats.effect.std.Random
import org.scalacheck.{Arbitrary, Gen}

object RandomGen {

  implicit val funcGen: Functor[Gen] = new Functor[Gen] {
    override def map[A, B](fa: Gen[A])(f: A => B): Gen[B] =
      fa.map(f)
  }

  implicit val randomGen: Random[Gen] = new Random[Gen] {
    override def betweenDouble(
      minInclusive: Double,
      maxExclusive: Double
    ): Gen[Double] = ???

    override def betweenFloat(
      minInclusive: Float,
      maxExclusive: Float
    ): Gen[Float] = ???

    override def betweenInt(minInclusive: Int, maxExclusive: Int): Gen[Int] =
      ???

    override def betweenLong(
      minInclusive: Long,
      maxExclusive: Long
    ): Gen[Long] = ???

    override def nextAlphaNumeric: Gen[Char] = ???

    override def nextBoolean: Gen[Boolean] = ???

    override def nextBytes(n: Int): Gen[Array[Byte]] = ???

    override def nextDouble: Gen[Double] = ???

    override def nextFloat: Gen[Float] = ???

    override def nextGaussian: Gen[Double] = ???

    override def nextInt: Gen[Int] = ???

    override def nextIntBounded(n: Int): Gen[Int] = ???

    override def nextLong: Gen[Long] = ???

    override def nextLongBounded(n: Long): Gen[Long] = ???

    override def nextPrintableChar: Gen[Char] = ???

    override def nextString(length: Int): Gen[String] = ???

    override def shuffleList[A](l: List[A]): Gen[List[A]] =
      for {
        shuffledList <- l
          .map(x => (Arbitrary.arbitrary[Int].sample, x))
          .sortBy(_._1)
          .map(_._2)
      } yield shuffledList

    override def shuffleVector[A](v: Vector[A]): Gen[Vector[A]] = ???
  }
}
