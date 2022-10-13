name := "utils"

version := "0.1"

scalaVersion := "2.13.8"

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

//libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"