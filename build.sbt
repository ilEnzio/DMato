name := "DMato"

version := "0.1"

scalaVersion := "2.13.8"
val catsVersion = "2.7.0"

libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.13" % "test"