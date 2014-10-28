/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.supervised.hmm


import org.scalaml.core.types
import types._
import org.scalaml.supervised.Supervised
import org.scalaml.core.design.{PipeOperator, Config}
import org.scalaml.core.XTSeries

import HMM._
import org.scalaml.util.Matrix



		/**
		 * <p>Utility class that defined the dimemsion of the matrix
		 * used in the Hidden Markov Model. The terminology used in the code follows
		 * the naming convention used in the mathematical expressions presented in
		 * most of papers and technical books on HMM as well as the book</p>
		 * @param _T number of observations
		 * @param _N number of hidden states in the HMM
		 * @param _M number of symbols (or model dimension) for the HMM
		 * @throws IllegalArgumenException if any of the argument is out of range [1, 1000]
		 * @author Patrick Nicolas
		 * @since March 27, 2014
		 */
class HMMConfig(val _T: Int, val _N: Int, val _M: Int) extends Config {
  require( _T > 0 && _T < 1000, "Number of observations " + _T + " in HMM lambda model is out of bounds")
  require( _N > 0 && _N < 1000, "Number of States " + _N + " in HMM lambda model is out of bounds")
  require( _M > 0 && _M < 1000, "Number of symbols " + _M + " in HMM lambda model is out of bounds")
  val persists = "config/hmm"
}

object HMMConfig {
   def foreach(i: Int, f: (Int) => Unit): Unit = Range(0, i).foreach(f)
   def foldLeft(i: Int, f: (Double, Int) => Double, zero:Double) = Range(0, i).foldLeft(zero)(f)
   def foldLeft(i: Int, f: (Double, Int) => Double) = Range(0, i).foldLeft(0.0)(f)
   
   def maxBy(i: Int, f: Int => Double): Int = Range(0, i).maxBy(f)
}




// ----------------------------------------  EOF ------------------------------------------------------------