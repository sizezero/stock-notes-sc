val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "stock-notes-sc",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    exportJars := true,

    //libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
    libraryDependencies += "org.scala-lang" %% "toolkit" % "0.1.7",
    libraryDependencies += "org.scala-lang" %% "toolkit-test" % "0.1.7" % Test
  )
