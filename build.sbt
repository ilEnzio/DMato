name := "DMato"

version := "0.1"

scalaVersion := "2.13.8"

val catsVersion   = "2.7.0"
val Http4sVersion = "1.0.0-M21"
val CirceVersion  = "0.14.0-M5"

Test / testOptions +=
  Tests.Argument(
    TestFrameworks.ScalaCheck,
    "-verbosity",
    "1",
    "-minSuccessfulTests",
    "10000"
  )

libraryDependencies ++= Seq(
  "org.typelevel"     %% "cats-core"           % catsVersion,
  "org.scalactic"     %% "scalactic"           % "3.2.13",
  "org.scalatest"     %% "scalatest"           % "3.2.13"  % Test,
  "org.scalatestplus" %% "scalacheck-1-14"     % "3.1.2.0" % Test,
  "org.scalacheck"    %% "scalacheck"          % "1.14.1"  % Test,
  "org.scalacheck"    %% "scalacheck"          % "1.14.1",
  "org.typelevel"     %% "cats-effect"         % "3.3.12",
  "org.http4s"        %% "http4s-blaze-server" % Http4sVersion,
  "org.http4s"        %% "http4s-circe"        % Http4sVersion,
  "org.http4s"        %% "http4s-dsl"          % Http4sVersion,
  "io.circe"          %% "circe-generic"       % CirceVersion
)
