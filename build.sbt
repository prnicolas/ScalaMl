name := "Scala for Machine Learning"

version := "0.80"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
"org.apache.commons" % "commons-math3" % "3.2",
"org.jfree" % "jfreechart" % "1.0.17",
"com.typesafe.akka" %% "akka-actor" % "2.1.0",
"org.apache.spark" %% "spark-core" % "0.9.0-incubating",
"iitb.CRF" % "CRF" % "1.0" from "http://sourceforge.net/projects/crf/files/latest/download")


