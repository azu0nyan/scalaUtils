ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

lazy val utilsLib = RootProject(file("../scalaUtils"))

lazy val utilsLib2 = RootProject(file("../scalaAwtDrawing"))

lazy val root = (project in file("."))
  .settings(
    name := "straightSkeleton"
  ).dependsOn(utilsLib)
  .dependsOn(utilsLib2 % "test->test")
