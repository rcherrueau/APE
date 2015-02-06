name := "Chap08 Property-based testing"

version := "0.1"

// For standard scala
scalaVersion := "2.11.5"

// // Shows expansion of implicits:
// scalacOptions += "-Xlog-implicits"

// scalacOptions += "-feature"

// Import RNG and State from chap06
unmanagedSourceDirectories in Compile +=
  baseDirectory.value / "../chap06-purely-functional-state"


// Libraries
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.1"
