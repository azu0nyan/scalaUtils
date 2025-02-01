

val scalaTest =
  Seq(
    "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  )


val commonSettings = Seq(
  scalaVersion := "3.6.2",
  version := "0.3",
  scalacOptions ++=
    Seq(
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
      //  "-Ytasty-reader",
    ),
  libraryDependencies ++= scalaTest,
)


lazy val core =
  (project in file("core"))
    .settings(
      commonSettings,
      name := "core",
    )

lazy val dcel = (project in file("dcel"))
  .settings(
    commonSettings,
    name := "dcel",
  )
  .dependsOn(core)

lazy val `dcel-nav` = (project in file("dcel-nav"))
  .settings(
    commonSettings,
    name := "dcel-nav",
  )
  .dependsOn(dcel)

lazy val triangulation = (project in file("triangulation"))
  .settings(
    commonSettings,
    name := "triangulation",
  )
  .dependsOn(dcel)

lazy val root = (project in file("."))
  .aggregate(core, dcel)
  .settings(
    commonSettings,
    name := "root",
  )

