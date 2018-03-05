import sbt._

object Dependencies {
  lazy val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"
  lazy val scalastic         = "org.scalactic"          %% "scalactic"                % "3.0.5"
  lazy val scalatest         = "org.scalatest"          %% "scalatest"                % "3.0.5"
  lazy val scalacheck        = "org.scalacheck"         %% "scalacheck"               % "1.13.5"
}
