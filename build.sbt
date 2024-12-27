name := "utils"

version := "0.3"

scalaVersion := "3.6.2"

scalacOptions ++= Seq(
  "-encoding", "utf8", // Option and arguments on same line
  //"-Xfatal-warnings",  // New lines for each options
//  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps",
// "-source:3.0-migration",
//  "-rewrite",
 // "-explain",
 // "--explain-types"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"