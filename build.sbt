name := "DMato"

version := "0.1"

scalaVersion := "2.13.8"

val catsVersion = "2.7.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.scalatest" %% "scalatest" % "3.2.13" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.1" % "test")