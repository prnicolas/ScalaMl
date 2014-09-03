organization := "Patrick Nicolas"

name := "scalaml"

version := "0.91"

scalaVersion := "2.10.3"

// The build script assumes that the following jar file are
// included in the lib
// lib/colt.jar, CRF.jar, CRF-Trove_3.0.2.jar, LBFGS.jar and libsvm_sml.jar

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math3" % "3.3",
  "org.jfree" % "jfreechart" % "1.0.17",
  "com.typesafe.akka" %% "akka-actor" % "2.1.0",
  "org.apache.spark" %% "spark-core" % "1.0.0",
  "org.apache.spark" %% "spark-mllib" % "1.0.0",
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)


// Options for the Scala compiler should be customize
scalacOptions ++= Seq("-unchecked", "-optimize", "-Yinline-warnings", "-feature")

