ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.15"


lazy val utilsLib = RootProject(file("../scalaUtils"))

lazy val root = (project in file("."))
  .settings(
    name := "dcel"
  ).dependsOn(utilsLib)
