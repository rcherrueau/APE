name := "scala-illtyped"

version := "1.0"

scalaVersion := "2.11.2"

//libraryDependencies += "com.chuusai" %% "shapeless" % "2.0.0"

// scalaVersion := "2.11.2-typelevel"

// libraryDependencies += "org.scala-lang" % "scala-typelevel" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.2"

// resolvers += Resolver.mavenLocal

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)

