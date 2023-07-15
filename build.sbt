ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"
ThisBuild / libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
ThisBuild / libraryDependencies += "com.opencsv" % "opencsv" % "5.7.1"
lazy val root = (project in file("."))
  .settings(
    name := "sanavarastooppityokalu"
  )
