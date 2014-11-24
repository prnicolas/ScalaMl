ScalaMl
=======

Source code, data files and utilities related to "Scala for Machine Learning"


Version 0.96 Copyright Patrick Nicolas All rights reserved 2013-2015

<h2>Overview</h2>
The source code provides software developers with a broad overview of the difference in machine learning algorithms. The reader is expected to have a good grasp of the Scala programming language along with some knowledge in basic statistics. Experience in data mining and machine learning is not a pre-requisite.<br>

The examples are related to investment portfolio management and trading strategies. For the readers interested either in mathematics or the techniques implemented in this library, I strongly recommend the following readings:
<ul>
<li>"Machine Learning: A Probabilistic Perspective" K. Murphy</li>
<li>"The Elements of Statistical Learning" T. Hastie, R. Tibshirani, J. Friedman</li>
</ul>
The real-world examples, related to financial and market analysis, used for the sole purpose of illustrating the machine learning techniques. They do not constitute a recommendation or endorsement of any specific investment management or trading techniques.<br>

<h2>Minimum Requirements</h2>
<b>Hardware</b>: 2 CPU core with 4 Gbytes RAM for small datasets to build and run examples.<br>
4 CPU Core and 8+ Gbytes RAM for datasets of size 75,000 or larger and/or with 50 features set or larger
<br>
<b>Operating system</b>: None<br>
<b>Software</b>: JDK 1.7.0_45 or 1.8.0_25, Scala 2.10.3/2.10.4 or 2.11.2 and SBT 0.13+ (see installation section for deployment.<br>

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
Build script for <i>Scala for Machine Learning</i>:<br>
To build the library and tests: <i>$(ROOT)/sbt clean compile publish-local</i><br>
<br>


<h2>Installation and Build</h2>
The Simple Build Too (SBT) has to be used to build the library from the source code using the <i>build.sbt</i> file in the root directory: <i>sbt compile publish-local</i><br>
The installation and build workflow is described in the following diagram:<br>
<img src="images/8742OS_installation.png" alt="Installation and build"><br>

