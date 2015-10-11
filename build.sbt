import sbt.Keys._

def commonSettings = Seq(
  organization := "com.jsuereth",
  version := "0.1-SNAPSHOT",
  isSnapshot := true,
  scalaVersion := "2.11.6",
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0-M9" % "test"
)

lazy val root =
  (project in file(".")).aggregate(
    viewductionJVM, viewductionJS
  ).settings(
    publishTo := None,
    publishLocal := ()
  )

lazy val viewduction =
  (crossProject.crossType(CrossType.Pure) in file(".")).settings(
    name := "viewduction"
  ).settings(commonSettings:_*)

lazy val viewductionJVM = viewduction.jvm
lazy val viewductionJS = viewduction.js

lazy val benchmarks =
  (project in file("benchmark")).settings(
    name := "viewduction-benchmarks",
    libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.6",
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    logBuffered := false,
    parallelExecution in Test := false
  ).settings(commonSettings:_*).dependsOn(viewductionJVM)
