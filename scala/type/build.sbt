name := "scala-types"

version := "1.0"

// For standard scala
scalaVersion := "2.11.2"

// // For typelevel fork of scala
// scalaVersion := "2.11.2-typelevel"

// resolvers += Resolver.mavenLocal


libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

unmanagedJars in Compile ++=
  (file("utils/target/scala-2.11/") * "scala-illtyped_2.11-1.0.jar").classpath
