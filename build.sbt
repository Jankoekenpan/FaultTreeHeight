ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

enablePlugins(JmhPlugin)

lazy val root = (project in file("."))
  .settings(
    name := "FaultTreeHeight"
  )

javacOptions ++= Seq("--release", "23", "--enable-preview")
javaOptions += "--enable-preview"