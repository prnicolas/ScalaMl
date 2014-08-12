/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 * 
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
				  val paramsV: HMMParams, 
				  val obsV: Array[Int]) extends HMMInference(lambdaV, paramsV, obsV) {
	  /**
	   * Maximum value for delta computed by recursion. the computation
	   * throws a Arithmetic or Runtime exception that is to be caught by
	   * the client code
	   */
   val maxDelta = recurse(lambda.d._T, 0)

   private def initial(ti: (Int, Int)): Double = {
  	 if(ti._1 == 0) {
  	    params.psi += (0, 0, 0)
  		params.delta(ti._1, ti._2) + lambda.pi(ti._2) * lambda.B(ti._1, labels(ti._2))
  	 }
  	 else 
  		-1.0
   }
   
   private def recurse(t: Int, j: Int): Double = {
  	 	// Initialization of the delta value, return -1.0 in case of error
  	 var maxDelta = initial((t, j))
  	 
  	 if( maxDelta == -1.0) {
  	    if( t != labels.size) {
            maxDelta = lambda.d.rn.maxBy( s =>  
  		        recurse(t-1, s)* lambda.A(s, j)* lambda.B(j, labels(t)) )	   
  		        
  		    val idx = lambda.d.rt.maxBy(i =>recurse(t-1 ,i)*lambda.A(i,j))
  		    params.psi += (t, j, idx)
            params.delta += (t, j, maxDelta)
  	     }
  	     else {
  		   maxDelta = 0.0  		   
  		   val index = lambda.d.rn.maxBy( i => { 
  		  	   val delta = recurse(t-1 ,i)
  		  	   if( delta > maxDelta) maxDelta = delta
  		  	   delta
  		   })
  		    params.QStar.update(t, index)
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
	def apply(lambda: HMMLambda, params: HMMParams, _labels: Array[Int]): ViterbiPath = new ViterbiPath(lambda, params, _labels)
}



// -------------------------  EOF ----------------------------------------------