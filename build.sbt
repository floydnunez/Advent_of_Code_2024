ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.1"

lazy val root = (project in file("."))
  .settings(
    name := "Advent_2024",
  )

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
