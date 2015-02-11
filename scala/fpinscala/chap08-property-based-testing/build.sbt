name := "Chap08 Property-based testing"

version := "0.1"

// For standard scala
scalaVersion := "2.11.5"

// // Shows expansion of implicits:
// scalacOptions += "-Xlog-implicits"

// scalacOptions += "-feature"

// Import Stream from chap05
unmanagedSourceDirectories in Compile +=
  baseDirectory.value / "../chap05-strictness-and-laziness"

// Import RNG and State from chap06
unmanagedSourceDirectories in Compile +=
  baseDirectory.value / "../chap06-purely-functional-state"

// Import Par library from chap07
unmanagedSourceDirectories in Compile +=
  baseDirectory.value / "../chap07-Purely-functional-parallelism"


// Libraries
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.1"
