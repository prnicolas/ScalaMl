<b>Scala for Machine Learning</b> Version <b>0.98</b><br>
Copyright Patrick Nicolas All rights reserved 2013-2015<br>
=================================================================<br>
Source code, data files and utilities related to "Scala for Machine Learning"<br>
<a href="#overview">Overview</a><br>
<a href="#requirements">Minimum requirements</a><br>
<a href="#documentation">Documentation</a><br>
<a href="#project">Project</a><br>
<a href="#installation">installation</a><br>
<a href="#build">build</a><br>
<a href="#run">Run examples</a><br>
<a href="#persistency">Persistent models and configurations</a><br>
<a href="#appendix">Appendix</a><br>

<h2 id="overview">Overview</h2>
The source code provides software developers with a broad overview of the difference in machine learning algorithms. The reader is expected to have a good grasp of the Scala programming language along with some knowledge in basic statistics. Experience in data mining and machine learning is not a pre-requisite.<br><br>
Source code guidelines are defined in the companion document <b>SourceCodeGuide.html</b><br>

The examples are related to investment portfolio management and trading strategies. For the readers interested either in mathematics or the techniques implemented in this library, I strongly recommend the following readings:
<ul>
<li>"Machine Learning: A Probabilistic Perspective" K. Murphy</li>
<li>"The Elements of Statistical Learning" T. Hastie, R. Tibshirani, J. Friedman</li>
</ul>
The real-world examples, related to financial and market analysis, used for the sole purpose of illustrating the machine learning techniques. They do not constitute a recommendation or endorsement of any specific investment management or trading techniques.<br>
The Appendix contains an introduction to the basic concepts of investment and trading strategies as well as technical analysis of financial markets. 

<h2 id="Documentation">Documentation</h2>
The best approach to learn about any particular learning algorithm is to 
<ul>
<li>Read the appropriate chapter (i.e. <i>Chapter 5: Naive Bayes modelsM</i>)</li>
<li>Review source code guidelines used in the book <i>SourceCodeGuide.html</i></li>
<li>Review scaladoc in scala_2.10-0.98-javadoc.jar</li>
<li>Look at the examples related to the chapter (i.e. <i>org/scalaml/app/chap5/Chap5</i>)</li>
<li>Browse through the implementation code (i.e. <i>org/scalaml/supervised/bayes</i>)</li>
</ul>
<h2 id="requirements">Minimum Requirements</h2>
<b>Hardware</b>: 2 CPU core with 4 Gbytes RAM for small datasets to build and run examples.<br>
4 CPU Core and 8+ Gbytes RAM for datasets of size 75,000 or larger and/or with 50 features set or larger
<br>
<b>Operating system</b>: None<br>
<b>Software</b>: JDK 1.7.0_45 or 1.8.0_25, Scala 2.10.3/2.10.4 or 2.11.1 and SBT 0.13+ (see installation section for deployment.<br>

<h2 id="project">Project Components</h2>
Directory structure of the source code library for <i>Scala for Machine Learning</i>:<br>
<img src="images/8742OS_libsourcecode.png" alt="Source code"><br>
<br>
Directory structure of the source code of the examples for <i>Scala for Machine Learning</i>:<br>
<img src="images/8742OS_examples.png" alt="Examples"><br>
<br>


<h2 id="installationbuild">Installation and Build</h2>
<h3 id="installation">Installation</h3>
The installation and build workflow is described in the following diagram:<br>
<img src="images/8742OS_installation.png" alt="Installation and build"><br>
<b>Eclipse</b>
The Scala for Machine Learning library is compatible with Eclipse Scala IDE 3.0<br>
Specify link to the source in <i>Project/properties/Java Build Path/Source</i>. The two links should be <i>project_name/src/main/scala</i> and <i>project_name/src/test/scala</i><br>
Add the jars required to build and execute the code within Eclipse <i>Project/properties/Java Build Path/Add External Jars</i>as declared in the <i>project_name/.classpath</i><br>
Update the JVM heap parameters in eclipse.ini file as <i>-Xms512m -Xmx8192m</i> or the maximum allowed on your specific machine.
<br>
<h3 id="build">Build</h3>
The Simple Build Too (SBT) has to be used to build the library from the source code using the <i>build.sbt</i> file in the root directory<br>
Executing the examples/test in Scala for Machine Learning require sufficient JVM Heap memory (~2G):<br>
in <i>sbt/conf/sbtconfig.text</i> set Xmx to 2058m or higher, -XX:MaxPermSize to 512m or higher i.e. <b>-Xmx4096m -Xms512m -XX:MaxPermSize=512m</b><br><br>
Build script for <i>Scala for Machine Learning</i>:<br>
To build the Scala for Machine Learning library package<br><i><b> $(ROOT)/sbt clean publish-local</b></i><br>
To build the package including test and resource files<br><i><b> $(ROOT)/sbt clean package</b></i><br>
To generate scala doc for the library<br><i><b> $(ROOT)/sbt doc</b></i><br>
To generate scala doc for the examples<br><i><b> $(ROOT)/sbt test:doc</b></i><br>
To compile all examples:<br><i><b>$(ROOT)/sbt test:compile</b></i><br>
<h2 id="run">Run examples</h2>
<h3>examples in a chapter</h3>
To run the examples of a particular chapter (i.e. Chapter 4)<br>
<b>$sbt<br>
&#62;test-only org.scalaml.app.chap4.Chap4</b>
<h3>All examples</h3>
To run all examples with output configuration:<br>
$<b>sbt "test:run options"</b> where options is a list of possible outputs<ul>
<li><b>console</b> to output results onto standard output</li> 
<li><b>logger</b> to output results into a log file (log4j)</li>
<li><b>chart</b> to plot results using <i>jFreeChart</i></li>
</ul>
$<b>sbt "test:run log chart"</b> write test results into a log and charts<br>
$<b>sbt test:run</b> write test results into the standard output and the charts.<br>

<h2 id="persistency">Persistent models and configurations</h2>
The package object <b>org.scalaml.core.Design</b> provide the trait (or skeleton implementation) of the persistent model <b>Design.Model</b> and configuration <b>Design.Config</b>.<br>
The persistency mechanisms is implemented for a couple of supervised learning models only for illustration purpose. The reader should be able to implement the persistency for configuration and models for all relevant learning algorithms using the template operator << and >> <br>

<h2 id="appendix">Appendix</h2>
The examples have been built and tested with the following libraries:<br>
<b>Java libraries</b><br>
CRF-Trove_3.0.2.jar<br>
LBFGS.jar<br>
colt.jar<br>
CRF.jar<br>
commons-math3-3.3.jar<br>
libsvm.jar<br>
jfreechart-1.0.17/lib/jcommon-1.0.21.jar<br>
jfreechart-1.0.17/lib/servlets.jar<br>
junit-4.11.jar<br>
jfreechart-1.0.17/lib/jfreechart-1.0.17.jar<br>
<b>Scala 2.10.x related libraries</b><br>
com.typesafe/config/1.2.1/bundles/config.jar<br>
akka-actor_2.10-2.2.3.jar<br>
scalatest_2.11.jar<br>
spark-assembly-1.0.2-hadoop2.4.0.jar<br>
<b>Scala 2.11.x related libraries</b><br>
com.typesafe/config/1.2.2/bundles/config.jar<br>
scalatest_2.11.jar<br>
akka-actor_2.11-2.3.6.jar<br>
spark-assembly-1.1.0-hadoop2.4.0.jar

