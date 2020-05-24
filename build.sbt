name := "utils"

version := "0.1"

scalaVersion := "2.13.2"

scalacOptions ++= Seq(
  "-encoding", "utf8", // Option and arguments on same line
  "-Xfatal-warnings",  // New lines for each options
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps"
)

libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.1.2" % "test"