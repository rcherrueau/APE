name := "towards scalaz"

scalaVersion := "2.11.2"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

// libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.0.0"
)

// libraryDependencies += "com.chuusai" % "shapeless_2.11.0-RC4" % "2.0.0"
