val scala3Version = "3.1.0"
val circeVersion = "0.15.0-M1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "mail-user-agent",
    version := "0.1.0",

    mainClass := Some("mua.app"),
    scalaVersion := scala3Version,

    libraryDependencies += "com.github.hypfvieh" % "dbus-java" % "3.0.2",

    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),

  )
