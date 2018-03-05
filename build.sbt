import Dependencies._

name := "kll-parser"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  parserCombinators,
  scalastic,
  scalatest % "test",
  scalacheck % "test"
)

val unusedWarnings = "-Ywarn-unused" ::
  "-Ywarn-unused-import" ::
  Nil

scalacOptions ++= (
  "-deprecation" ::
    "-unchecked" ::
    "-Xlint" ::
    "-Xfuture" ::
    "-language:existentials" ::
    "-language:higherKinds" ::
    "-language:implicitConversions" ::
    "-Yno-adapted-args" ::
    Nil
  ) ::: unusedWarnings

Seq(Compile, Test).flatMap(c =>
  scalacOptions in (c, console) --= unusedWarnings)

enablePlugins(ScalafmtPlugin)
