name := "viewduciton"
organization := "com.jsuereth"
version := "0.1-SNAPSHOT"
isSnapshot := true
scalaVersion := "2.11.2"


// Tests
resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.6"

testFrameworks += new TestFramework(
  "org.scalameter.ScalaMeterFramework")

logBuffered := false

parallelExecution in Test := false