name := "scalaAwtDrawing"

version := "0.2"

scalaVersion := "2.13.2"


scalacOptions ++= Seq(
  "-encoding", "utf8", // Option and arguments on same  line
  "-Xfatal-warnings",  // New lines for each options
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps"
)



lazy val utilsLib = RootProject(file("../scalaUtils"))
val main = Project(id = "application", base = file(".")).dependsOn(utilsLib)