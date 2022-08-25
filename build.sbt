name := "DMato"

version := "0.1"

scalaVersion := "2.13.8"

val catsVersion = "2.7.0"

Test / testOptions +=
  Tests.Argument(
    TestFrameworks.ScalaCheck,
    "-verbosity",
    "1",
    "-minSuccessfulTests",
    "10000"
  )
libraryDependencies ++= Seq(
  "org.typelevel"     %% "cats-core"       % catsVersion,
  "org.scalactic"     %% "scalactic"       % "3.2.13",
  "org.scalatest"     %% "scalatest"       % "3.2.13"  % "test",
  "org.scalatestplus" %% "scalacheck-1-14" % "3.1.2.0" % Test,
  "org.scalacheck"    %% "scalacheck"      % "1.14.1"  % "test",
  "org.typelevel"     %% "cats-effect"     % "3.3.12"
)
