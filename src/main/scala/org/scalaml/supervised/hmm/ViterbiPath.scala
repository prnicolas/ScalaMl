/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.supervised.hmm


import org.scalaml.util.Matrix
import org.scalaml.core.Types


import Types._


	/**
	 * <p>Class that implements the Viterbi algorithm to extract the best sequence
	 * of hidden states in a HMM given a lambda model and a sequence of integer
	 * observations. The maximum value of delta is computed recursively during 
	 * instantiation. <br>
	 * The recursion throws a Arithmetic or Runtime exception that is to be caught by
	 * the client code</p>
	 *  @param lambdaV Lambda (pi, A, B) model for the HMM
	 *  @param paramsV parameters used in any of the three canonical form of the HMM
     *  @param  obsV Array of observations as integer (categorical data)
     *  @throws IllegalArgumentException if lambda, params and observations are undefined of eps is out of range
     *  @see org.scalaml.hmm.DynamicAlgorithm
	 *  @author Patrick Nicolas
	 *  @since March 17, 2014
	 *  @note Scala for Machine Learning
	 */
class ViterbiPath(val lambdaV: HMMLambda, 
				  val configV: HMMConfig, 
				  val obsV: Array[Int]) extends HMMInference(lambdaV, configV, obsV) {
	  /**
	   * Maximum value for delta computed by recursion. the computation
	   * throws a Arithmetic or Runtime exception that is to be caught by
	   * the client code
	   */
   val maxDelta = recurse(lambda.dim._T, 0)

   private def initial(ti: (Int, Int)): Double = {
  	 if(ti._1 == 0) {
  	    config.psi += (0, 0, 0)
  		config.delta(ti._1, ti._2) + lambda.pi(ti._2) * lambda.B(ti._1, labels(ti._2))
  	 }
  	 else 
  		-1.0
   }
   
   private def recurse(t: Int, j: Int): Double = {
  	 	// Initialization of the delta value, return -1.0 in case of error
  	 var maxDelta = initial((t, j))
  	 
  	 if( maxDelta == -1.0) {
  	    if( t != labels.size) {
  	    	maxDelta = HMMDim.maxBy(lambda.dim._N, s => recurse(t-1, s)* lambda.A(s, j)* lambda.B(j, labels(t)) )
  		    val idx = HMMDim.maxBy(lambda.dim._T, i =>recurse(t-1 ,i)*lambda.A(i,j))
  		    config.psi += (t, j, idx)
            config.delta += (t, j, maxDelta)
  	     }
  	     else {
  		   maxDelta = 0.0  		   
  		   val index = HMMDim.maxBy(lambda.dim._N, i => { 
  		  	   val delta = recurse(t-1 ,i)
  		  	   if( delta > maxDelta) maxDelta = delta
  		  	   delta
  		   })
  		    config.QStar.update(t, index)
  	     }
  	 }
  	 maxDelta
   }
}


	/**
	 * Object companion for the Viterbi algorithm for the extraction of 
	 * best sequences. Implements the constructor - apply
	 * @author Patrick Nicolas
	 * @since March 17, 2014
	 */
object ViterbiPath {
	def apply(lambda: HMMLambda, config: HMMConfig, _labels: Array[Int]): ViterbiPath = new ViterbiPath(lambda, config, _labels)
}



// -------------------------  EOF ----------------------------------------------