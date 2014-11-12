organization := "Patrick Nicolas"

name := "scalaml"

version := "0.95"

scalaVersion := "2.11.2"

// The build script assumes that the following jar file are
// included in the lib
// lib/colt.jar, CRF.jar, CRF-Trove_3.0.2.jar, LBFGS.jar and libsvm_sml.jar

libraryDependencies ++= Seq(
   "org.apache.commons" % "commons-math3" % "3.3",
   "org.jfree" % "jfreechart" % "1.0.17",
   "com.typesafe.akka" %% "akka-actor" % "2.3.6",
   "org.apache.spark" % "spark-core_2.10" % "1.0.2",
   "org.apache.spark" % "spark-mllib_2.10" % "1.0.2",
   "org.scalatest" %% "scalatest" % "2.1.6"
)

// Options for the Scala compiler should be customize
scalacOptions ++= Seq("-unchecked", "-optimize")

