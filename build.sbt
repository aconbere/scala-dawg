import AssemblyKeys._

assemblySettings

name := "dawg"

version := "0.0.1"

scalaVersion := "2.10.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature"
)

fork in run := true

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
  "junit" % "junit" % "4.10" % "test",
  "org.conbere" %% "irc" % "0.2.0",
  "org.apache.logging.log4j" % "log4j-core" % "2.0-beta3",
  "com.typesafe" %% "scalalogging-log4j" % "1.0.1"
)

testOptions in Test += Tests.Argument("-oDF")
