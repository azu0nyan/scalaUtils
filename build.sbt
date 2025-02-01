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

lazy val `awt-drawing` = (project in file("awt-drawing"))
  .settings(
    commonSettings,
    name := "awt-drawing",
  )
  .dependsOn(
    core
  )

lazy val `straight-skeleton` = (project in file("straight-skeleton"))
  .settings(
    commonSettings,
    name := "straight-skeleton",
  )
  .dependsOn(
    core,
    `awt-drawing` % "test->test"
  )

lazy val `behaviour-tree` = (project in file("behaviour-tree"))
  .settings(
    commonSettings,
    name := "behaviour-tree",
  )

lazy val root = (project in file("."))
  .aggregate(core, dcel, triangulation, `straight-skeleton`, `awt-drawing`, `behaviour-tree`)
  .settings(
    commonSettings,
    name := "root",
  )

