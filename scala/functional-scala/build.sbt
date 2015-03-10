name := "temp test"

version := "1.0"

// For standard scala
scalaVersion := "2.11.5"

// Shows expansion of implicits:
// scalacOptions += "-Xlog-implicits"

// scalacOptions += "-feature"

// Only include those files in the compilation
includeFilter in unmanagedSources := "DependentType.scala"


// Libraries
libraryDependencies +=  "com.chuusai" %% "shapeless" % "2.1.0"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

// illTyped macro
unmanagedJars in Compile ++=
  (file("utils/target/scala-2.11/") * "scala-illtyped_2.11-1.0.jar").classpath
