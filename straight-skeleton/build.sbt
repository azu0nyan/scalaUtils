ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.2"


lazy val utilsLib = RootProject(file("../scalaUtils"))

lazy val root = (project in file("."))
  .settings(
    name := "straightSkeleton"
  ).dependsOn(utilsLib)
