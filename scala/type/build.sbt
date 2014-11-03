name := "scala-types"

version := "1.0"

// For standard scala
scalaVersion := "2.11.2"

// // For typelevel fork of scala
// scalaVersion := "2.11.2-typelevel"

// resolvers += Resolver.mavenLocal


libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies +=  "com.chuusai" %% "shapeless" % "2.0.0"

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "1.4.0"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

libraryDependencies += "org.spire-math" %% "spire" % "0.8.2"

unmanagedJars in Compile ++=
  (file("utils/target/scala-2.11/") * "scala-illtyped_2.11-1.0.jar").classpath
