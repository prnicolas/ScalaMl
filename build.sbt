organization := "Patrick Nicolas"

name := "ScalaMl"

version := "0.98"

scalaVersion := "2.10.4"

// The build script assumes that the following jar file are
// included in the lib
// lib/colt.jar, CRF.jar, CRF-Trove_3.0.2.jar, LBFGS.jar and libsvm_sml.jar

libraryDependencies ++= Seq(
   "org.apache.commons" % "commons-math3" % "3.3",
   "org.jfree" % "jfreechart" % "1.0.17",
   "com.typesafe.akka" %% "akka-actor" % "2.2.3",
   "org.apache.spark" %% "spark-core" % "1.0.2",
   "org.apache.spark" %% "spark-mllib" % "1.0.2",
   "org.scalatest" %% "scalatest" % "2.1.6"
)

resolvers += "Akka Repository" at "http://repo.akka.io/releases/"

// Options for the Scala compiler should be customize
scalacOptions ++= Seq("-unchecked", "-optimize", "-Yinline-warnings")

scalacOptions in (Compile, doc) ++= Seq("-doc-root-content", baseDirectory.value+"/root-doc.txt")

// Options for Scala test with timing and short stack trace
testOptions in Test += Tests.Argument("-oDS")
