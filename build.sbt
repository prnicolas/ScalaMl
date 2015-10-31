organization := "Patrick Nicolas"

name := "ScalaMl"

version := "0.99"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
   "org.apache.commons" % "commons-math3" % "3.5",
   "org.jfree" % "jfreechart" % "1.0.17",
   "com.typesafe.akka" %% "akka-actor" % "2.3.4",
   "org.apache.spark" %% "spark-core" % "1.5.0",
   "org.apache.spark" %% "spark-mllib" % "1.5.0",
   "org.scalatest" %% "scalatest" % "2.2.2"
)

// Resolver for Apache Spark framework
resolvers += "Akka Repository" at "http://repo.akka.io/releases/"

// Options for the Scala compiler should be customize
scalacOptions ++= Seq("-unchecked", "-optimize", "-language:postfixOps")

// Specifies the content for the root package used in Scaladoc
scalacOptions in (Compile, doc) ++= Seq("-doc-root-content", baseDirectory.value+"/root-doc.txt")

excludeFilter in doc in Compile := "*.java" || "*.jar"

// Set the path to the unmanaged, 3rd party libraries
unmanagedClasspath in Compile ++= Seq(
  file("lib/CRF-1.1.jar"), 
  file("lib/Trove-3.0.2.jar"), 
  file("lib/colt.jar"), 
  file("lib/libsvm_sml-3.18.jar") 
)

// Options for Scala test with timing and short stack trace
testOptions in Test += Tests.Argument("-oDS")

// Maximum number of errors during build
maxErrors := 30


