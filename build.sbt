ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "3.1.3"

ThisBuild / scalacOptions ++= List("-feature", "-deprecation", "-Ykind-projector:underscores", "-source:future")

ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test

lazy val root = (project in file("."))
  .settings(
    name := "learn-fp-in-scala"
  )
