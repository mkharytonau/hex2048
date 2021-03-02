import Dependencies._

enablePlugins(ScalaJSPlugin)

ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

val circeVersion = "0.13.0"

lazy val root = (project in file("."))
  .settings(
    name := "hex2048",
    libraryDependencies ++= Seq(
    	"org.scala-js" %%% "scalajs-dom" % "1.1.0",

      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-generic" % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion,

    	"org.scalatest" %%% "scalatest" % "3.2.2" % Test,
    	"org.scalatestplus" %%% "scalacheck-1-15" % "3.2.2.0" % Test,
    )
  )

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
