scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "3.0.0-M1",
  "org.slf4j" % "slf4j-nop" % "1.6.4"
)

libraryDependencies += "com.h2database" % "h2" % "1.3.148"
