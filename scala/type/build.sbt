name := "scala-types"

version := "1.0"

// scalaVersion := "2.11.2"

//libraryDependencies += "com.chuusai" %% "shapeless" % "2.0.0"


scalaVersion := "2.11.2-typelevel"

// libraryDependencies += "org.scala-lang" % "scala-typelevel" % scalaVersion.value

resolvers += Resolver.mavenLocal

unmanagedJars in Compile ++=
  (file("utils/target/scala-2.11/") * "scala-illtyped_2.11-1.0.jar").classpath
