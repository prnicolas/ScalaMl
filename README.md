Scala for Machine Learning Version 0.96a Copyright Patrick Nicolas All rights reserved 2013-2015<br>
=================================================================================================<br>
Source code, data files and utilities related to "Scala for Machine Learning"

<h2>Overview</h2>
The source code provides software developers with a broad overview of the difference in machine learning algorithms. The reader is expected to have a good grasp of the Scala programming language along with some knowledge in basic statistics. Experience in data mining and machine learning is not a pre-requisite.<br>

The examples are related to investment portfolio management and trading strategies. For the readers interested either in mathematics or the techniques implemented in this library, I strongly recommend the following readings:
<ul>
<li>"Machine Learning: A Probabilistic Perspective" K. Murphy</li>
<li>"The Elements of Statistical Learning" T. Hastie, R. Tibshirani, J. Friedman</li>
</ul>
The real-world examples, related to financial and market analysis, used for the sole purpose of illustrating the machine learning techniques. They do not constitute a recommendation or endorsement of any specific investment management or trading techniques.<br>
The Appendix contains an introduction to the basic concepts of investment and trading strategies as well as technical analysis of financial markets. 

<h2>Minimum Requirements</h2>
<b>Hardware</b>: 2 CPU core with 4 Gbytes RAM for small datasets to build and run examples.<br>
4 CPU Core and 8+ Gbytes RAM for datasets of size 75,000 or larger and/or with 50 features set or larger
<br>
<b>Operating system</b>: None<br>
<b>Software</b>: JDK 1.7.0_45 or 1.8.0_25, Scala 2.10.3/2.10.4 or 2.11.1 and SBT 0.13+ (see installation section for deployment.<br>

<h2>Project Components</h2>
Directory structure of the source code library for <i>Scala for Machine Learning</i>:<br>
<img src="images/8742OS_libsourcecode.png" alt="Source code"><br>
<br>
Directory structure of the source code of the examples for <i>Scala for Machine Learning</i>:<br>
<img src="images/8742OS_examples.png" alt="Examples"><br>
<br>
Library components for <i>Scala for Machine Learning</i>:<br>
<img src="images/8742OS_lib.png" alt="Libraries"><br>
<br>


<h2>Installation and Build</h2>
<h3>Installation</h3>
The installation and build workflow is described in the following diagram:<br>
<img src="images/8742OS_installation.png" alt="Installation and build"><br>
<b>Eclipse</b>
The Scala for Machine Learning library is compatible with Eclipse Scala IDE 3.0<br>
Specify link to the source in <i>Project/properties/Java Build Path/Source</i>. The two links should be <i>project_name/src/main/scala</i> and <i>project_name/src/test/scala</i><br>
Add the jars required to build and execute the code within Eclipse <i>Project/properties/Java Build Path/Add External Jars</i>as declared in the <i>project_name/.classpath</i><br>
Update the JVM heap parameters in eclipse.ini file as <i>-Xms512m -Xmx8192m</i> or the maximum allowed on your specific machine.
<br>
<h3>Build</h3>
The Simple Build Too (SBT) has to be used to build the library from the source code using the <i>build.sbt</i> file in the root directory<br>
Executing the examples/test in Scala for Machine Learning require sufficient JVM Heap memory (~2G):<br>
in <i>sbt/conf/sbtconfig.text</i> set Xmx to 2058m or higher, -XX:MaxPermSize to 512m or higher i.e. <b>-Xmx4096m -Xms512m -XX:MaxPermSize=512m</b><br><br>
Build script for <i>Scala for Machine Learning</i>:<br>
To build the Scala for Machine Learning library package<br><i><b> $(ROOT)/sbt clean publish-local</b></i><br>
To build the package including test and resource files<br><i><b> $(ROOT)/sbt clean package</b></i><br>
To generate scala doc for the library<br><i><b> $(ROOT)/sbt doc</b></i><br>
To generate scala doc for the examples<br><i><b> $(ROOT)/sbt test:doc</b></i><br>
To compile all examples:<br><i><b>$(ROOT)/sbt test:compile</b></i><br>
To run one test suite (i.e. Chap 3)<br>
<i><b>$(ROOT)/sbt<br>
> test-only *Chap3</b></i>
To run all tests:<i><b>$(ROOT)/sbt test:run</b></b></i><br>
<br>

<h2>Appendix</h2>
<h3>List of Jar files for Eclipse/Scala IDE setup</h3>
CRF-Trove_3.0.2.jar<br>
LBFGS.jar<br>
colt.jar<br>
CRF.jar<br>
commons-math3-3.3.jar<br>
libsvm.jar<br>
jfreechart-1.0.17/lib/jcommon-1.0.21.jar<br>
junit-4.11.jar<br>
jfreechart-1.0.17/lib/jfreechart-1.0.17.jar<br>
com.typesafe/config/1.2.1/bundles/config.jar<br>
jfreechart-1.0.17/lib/servlets.jar<br>
akka-actor_2.11-2.3.6.jar<br>
scalatest_2.11.jar<br>
spark-assembly-1.1.0-hadoop2.4.0-no_scala.jar
