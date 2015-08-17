import sbt.Keys._

def commonSettings = Seq(
  organization := "com.jsuereth",
  version := "0.1-SNAPSHOT",
  isSnapshot := true,
  scalaVersion := "2.11.6",
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  libraryDependencies += "org.specs2" %% "specs2-core" % "2.4.14" % "test"
)

lazy val viewduction =
  (project in file(".")).settings(
    name := "viewduction"
  ).settings(commonSettings:_*)

lazy val benchmarks =
  (project in file("benchmark")).settings(
    name := "viewduction-benchmarks",
    libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.6",
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    logBuffered := false,
    parallelExecution in Test := false
  ).settings(commonSettings:_*).dependsOn(viewduction)





