import Dependencies._


ThisBuild / scalaVersion     := "2.13.6"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
    .settings(
        name := "FFT-Scala",
        libraryDependencies ++= Seq(
        scalaTest % Test,
        "org.apache.commons" % "commons-math3" % "3.6.1",
        "org.scalaz" %% "scalaz-core" % "7.3.5",
        "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3"
        )
    )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
