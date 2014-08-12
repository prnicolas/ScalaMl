ScalaMl
=======

Source code, data files and utilities related to "Scala for Machine Learning"


Version 0.8 Copyright Patrick Nicolas All rights reserved 2013-2014

Overview
The source code provides software developers with a broad overview of the difference in machine learning algorithms. The reader is expected to have a good grasp of the Scala programming language along with some knowledge in basic statistics. Experience in data mining and machine learning is not a pre-requisite.

The examples are related to investment portfolio management and trading strategies. For the readers interested either in mathematics or the techniques implemented in this library, I strongly recommend the following readings
  "Machine Learning: A Probabilistic Perspective" K. Murphy 
  "The Elements of Statistical Learning" T. Hastie, R. Tibshirani, J. Friedman 
The real-world examples, related to financial and market analysis, used for the sole purpose of illustrating the machine learning techniques. They do not constitute a recommendation or endorsement of any specific investment management or trading techniques.


Build
The Simple Build Too (SBT) has to be used to build the library from the source code using the build.sbt file in the root directory
<i>sbt compile publish-local</i>
