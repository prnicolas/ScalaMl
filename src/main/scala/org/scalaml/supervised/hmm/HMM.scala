/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 * 
 */
package org.scalaml.supervised.hmm


import org.scalaml.core.Types
import Types._
import org.scalaml.supervised.Supervised
import org.scalaml.workflow.PipeOperator
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
case class HMMDim(val _T: Int, val _N: Int, val _M: Int) {
  require( _T > 0 && _T < 1000, "Number of observations " + _T + " in HMM lambda model is out of bounds")
  require( _N > 0 && _N < 1000, "Number of States " + _N + " in HMM lambda model is out of bounds")
  require( _M > 0 && _M < 1000, "Number of symbols " + _M + " in HMM lambda model is out of bounds")
   
  val rn = Range(0, _N)
  val rt = Range(0, _T)
  val rm = Range(0, _M)
    
  def foreachT( f: (Int) => Unit): Unit = rt.foreach(f)
  def foreachN( f: (Int) => Unit): Unit = rn.foreach(f)
  def foreachM( f: (Int) => Unit): Unit = rm.foreach(f)
}



class HMMAlgorithm(val lambda: HMMLambda, val labels: Array[Int]) {
   require(lambda != null, "Cannot execute dynammic algorithm with undefined HMM lambda model")
   require(labels != null && labels.size > 0, "Cannot execute dynammic algorithm  with undefined observations")
}


	/**
	 * <p>Generic class for the alpha (forward) pass and beta (backward) passes used in
	 * the evaluation form of the HMM.</p>
	 * @param lambda Lambda (pi, A, B) model for the HMM
	 * @param obs: Array of observations as integer (categorical data)
	 * @throws IllegalArgumentException if lambda, params and observations are undefined
	 * @author Patrick Nicolas
	 * @since March 29, 2014
	 */
class Pass(val lp: HMMLambda, val lbls: Array[Int]) extends HMMAlgorithm(lp, lbls) { 
   import Types.ScalaMl._
  
   protected var alphaBeta: Matrix[Double] = null
   protected val ct = Array.fill(lambda.d._T)(0.0)

   protected def normalize(t: Int): Unit = {
  	  require(t >= 0 && t < lambda.d._N, "Incorrect argument " + t + " for normalization")
  	  ct.update(t, lambda.d.rn.foldLeft(0.0)( (s, n) => s + alphaBeta(t, n)))
  	  alphaBeta /= (t, ct(t))
   }
}



	/**
	 * <p>Generic class for the Dynamic Programming algorithms used in the Hidden 
	 * Markov Model. The sub-classes are the alpha (forward) pass, beta (backward) pass,
	 * Viterbi algorithm the compute the best sequence of hidden states and the Baum-Welch
	 * algorithm used in training a HMM.</p>
	 * @param lambda Lambda (pi, A, B) model for the HMM
	 * @param params parameters (alpha, beta, gamma, digamma, q) used in any of the three
	 * canonical form of the HMM
	 * @param labels: Array of observations as integer (categorical data)
	 * @throws IllegalArgumentException if lambda, params and observations are undefined
	 * @author Patrick Nicolas
	 * @since March 7, 2014
	 */
class HMMInference(val li: HMMLambda, val params: HMMParams, val _labels: Array[Int]) extends HMMAlgorithm(li,  _labels) {
  require(params != null, "Cannot execute dynammic algorithm  with undefined HMM execution parameters")
}






	/**
	 * <p>Enumeration class to specify the canonical form of the HMM</p>
	 * @author Patrick Nicolas
	 * @since March 9, 2014
	 */
object HMMForm extends Enumeration {
  type HMMForm = Value
  val EVALUATION, DECODING = Value
}


	/**
	 * <p>Generic class for the Hidden Markov Model, parameterized with a view bounded to an
	 * array of integers. This class implement the classifier method defined in the PipeOperator
	 * trait and the validation method defined in the Supervised trait.</p>
	 * @param lambda lambda (pi, A, B) model for the HMM 
	 * @param form canonical form used in HMM (Evaluation or Decoding form)
	 * @param maxIter maximum number of iterations for training a HMM
	 * @throws IllegalArgumentException if lambda, form are undefined or if the maximum number of
	 * iterations is out of bounds
	 * @see org.scalaml.workflow.PipeOperator
	 * @author Patrick Nicolas
	 * @since March 11, 2014
	 */

import HMMForm._
class HMM[@specialized T <% Array[Int]](val lambda: HMMLambda, val form: HMMForm, val maxIters: Int)  
                           extends PipeOperator[T, HMMPredictor]
                                 with Supervised[T] {

	require(lambda != null, "Cannot execute a HMM with undefined lambda model")
	require(form != null, "Cannot execute a HMM with undefined canonical form")
	require( maxIters > 1 && maxIters < 1000, "Maximum number of iterations to train a HMM " + maxIters + " is out of bounds")
	
	protected val params = HMMParams(lambda.d, maxIters)
	
		/**
		 * <p>Classifier for the Hidden Markov Model. The pipe operator evaluates the 
		 * HMM if form == EVALUATION or decodes a HMM if form == DECODING for a given
		 * set of observations obs and a lambda model.</p>
		 * @param obs set of observation of type bounded by Array[Int]
		 * @return HMMPredictor instance if no computation error occurs, NONE otherwise
		 */
	def |> (obs: T): Option[HMMPredictor] = {
		require(obs != null, "Cannot perform an evaluaton or decoding of HMM with undefined observations")
		
		try {
	      form match {
		     case EVALUATION => Some(evaluate(obs))
		     case DECODING => Some(decode(obs))
		  }
		}
		catch {
		   case e: ArithmeticException => Console.println("HMM.|> " + e.toString); None
		   case e: RuntimeException => Console.println("HMM.|> " + e.toString); None
		   case e: Exception =>  Console.println("HMM.|> " + e.toString); None
		}
	}

		/**
		 * <p>Train HMM with a set of observations to extract the Lambda model.</p>
		 * @param  obs set of observation of type bounded by Array[Int]
		 * @return maximum log likelihood if no arithmetic function occurs, None otherwise
		 * @throws IllegalArgumentException if the set of observations is not defined
		 * @throws RuntimeException for computation error such as divide by zero
		 */
	def train(obs: T): Option[Double] = {
		require(obs != null, "Cannot train a HMM with undefined observations")
	    BaumWelchEM(lambda, params, obs).maxLikelihood
	}
	
	def validate(output: XTSeries[(Array[T], Int)], index: Int): Double = -1.0
		
	private def decode(obs: Array[Int]): HMMPredictor = (ViterbiPath(lambda, params, obs).maxDelta, params.QStar())
	
	private def evaluate(obs: Array[Int]): HMMPredictor = (-Alpha(lambda, obs).logProb, obs)
}



	/**
	 * <p>Companion object for the HMM that defines a HMMPredictor type and the constructor 
	 * apply for the HMM.</p>
	 * @author Patrick Nicolas
	 * @since March 11, 2014
	 */
object HMM {
	type HMMPredictor = (Double, Array[Int])
	def apply[T <% Array[Int]](lambda: HMMLambda, form: HMMForm, maxIters: Int): HMM[T] =  new HMM[T](lambda, form, maxIters)
	def apply[T <% Array[Int]](lambda: HMMLambda, form: HMMForm): HMM[T] =  new HMM[T](lambda, form, HMMParams.DEFAULT_MAXITERS)
}



// ----------------------------------------  EOF ------------------------------------------------------------