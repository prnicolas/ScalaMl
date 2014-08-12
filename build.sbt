organization := "Patrick Nicolas"

name := "ScalaMl"

version := "0.90"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
   "org.apache.commons" % "commons-math3" % "3.3",
   "org.jfree" % "jfreechart" % "1.0.17",
   "com.typesafe.akka" %% "akka-actor" % "2.1.0",
   "org.apache.spark" %% "spark-core" % "1.0.0",
   "org.apache.spark" %% "spark-mllib" % "1.0.0"
)


