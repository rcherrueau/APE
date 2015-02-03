name := "temp test"

version := "1.0"

// For standard scala
scalaVersion := "2.11.5"

// Shows expansion of implicits:
scalacOptions += "-Xlog-implicits"

scalacOptions += "-feature"

// Only include those files in the compilation
includeFilter in unmanagedSources := "OMO10a.scala"


// Libraries
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"
