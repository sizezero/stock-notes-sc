val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "stock-notes-sc",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    exportJars := true,

    // updated to latest scala three versions at maven central
    // on Jan 2024
    libraryDependencies += "org.scala-lang" %% "toolkit" % "0.2.1",
    libraryDependencies += "org.scala-lang" %% "toolkit-test" % "0.2.1" % Test,
    libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.12.0"
  )
