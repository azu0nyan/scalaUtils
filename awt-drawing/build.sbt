name := "scalaAwtDrawing"

version := "0.1"

scalaVersion := "3.6.2"


scalacOptions ++= Seq(
  "-encoding", "utf8", // Option and arguments on same  line
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps",
  "-Ytasty-reader",
)

autoScalaLibrary := true

lazy val utilsLib = RootProject(file("../scalaUtils"))
lazy val dcelLib = RootProject(file("../dcel"))
val main = Project(
  id = "application",
  base = file("."))
  .dependsOn(utilsLib)
//  .dependsOn(dcelLib) //todo