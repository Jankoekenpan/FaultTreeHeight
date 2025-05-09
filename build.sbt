ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

//rendering charts
ThisBuild / javaOptions ++= Seq(
    "--add-exports", "java.base/java.lang=ALL-UNNAMED",
    "--add-exports", "java.desktop/sun.awt=ALL-UNNAMED",
    "--add-exports", "java.desktop/sun.java2d=ALL-UNNAMED",
)

resolvers += "Jzy3d Releases" at "https://maven.jzy3d.org/releases/"
libraryDependencies += "org.jzy3d" % "jzy3d-everything" % "2.2.1"
libraryDependencies += "guru.nidi" % "graphviz-java" % "0.18.1"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.1.1"
libraryDependencies ++= Seq(
    "com.github.sbt.junit" % "jupiter-interface" % JupiterKeys.jupiterVersion.value % Test
)

enablePlugins(JmhPlugin)
Jmh / javaOptions ++= Seq(
    "-Djmh.resultsFormat=csv",
    "-Djmh.resultPath=benchmark-results.csv"
)

lazy val root = (project in file("."))
  .settings(
    name := "FaultTreeHeight"
  )

// structured concurrency api
javacOptions ++= Seq("--release", "23", "--enable-preview")
javaOptions += "--enable-preview"