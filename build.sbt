ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

ThisBuild / javaOptions ++= Seq(
    "--add-exports", "java.base/java.lang=ALL-UNNAMED",
    "--add-exports", "java.desktop/sun.awt=ALL-UNNAMED",
    "--add-exports", "java.desktop/sun.java2d=ALL-UNNAMED",
)

resolvers += "Jzy3d Releases" at "https://maven.jzy3d.org/releases/"
libraryDependencies += "org.jzy3d" % "jzy3d-everything" % "2.2.1"
libraryDependencies += "guru.nidi" % "graphviz-java" % "0.18.1"

enablePlugins(JmhPlugin)

lazy val root = (project in file("."))
  .settings(
    name := "FaultTreeHeight"
  )

javacOptions ++= Seq("--release", "23", "--enable-preview")
javaOptions += "--enable-preview"