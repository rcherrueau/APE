// import Dependencies._

lazy val root = (project in file(".")).
  settings(
    name := "Average",
    scalaVersion := "2.12.1",
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % "8.0.+",
      "com.typesafe" % "config" % "1.3.+"
    )
  )


// jar tf /Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home/jre/lib/ext/jfxrt.jar
unmanagedJars in Compile += {
  val ps = new sys.SystemProperties
  val jh = ps("java.home")
  Attributed.blank(file(jh) / "lib/ext/jfxrt.jar")
}
