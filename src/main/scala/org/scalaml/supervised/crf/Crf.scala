/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 * 
 * This code uses the iitb CRF library 
 * Copyright (c) <2004> <Sunita Sarawagi Indian Institute of Technology Bombay> All rights reserved.
 */
package org.scalaml.supervised.crf

import iitb.CRF.{CRF, CrfParams, DataSequence, DataIter, FeatureGenerator}
import iitb.Model.{FeatureGenImpl, CompleteModel}
import org.scalaml.core.XTSeries
import org.scalaml.workflow.data.DataSource
import org.scalaml.workflow.PipeOperator
import org.scalaml.supervised.Supervised
import java.io.IOException
import org.scalaml.core.Types.ScalaMl._

	/**
	 * <p>Class that defines the basic configuration of the CRF algorithm. The class generates a textual
	 * description of the configuration of CRF used by iitb library </p>
	 * @param initLambda  initial values for the CRF weights/factors (lambdas)
	 * @param maxIters  Maximum number of iterations to be used for the training of CRF
	 * @param l2Penalty L2-regularization penalty function 1/square(sigma) used in the log likelihood log p(Y|X)
	 * @param eps Covergence criteria used on the log likelihood  delta( log p(Y|X)to exit from the training iteration
	 * @param debug Optional debugging flag 
	 * 
	 * @author Patrick Nicolas
	 * @date April 3, 2014
	 */
case class CrfConfig(val initW: Double, val maxIters: Int, val lambda: Double, val eps:Double, val debug: Int = 0) {
	require( initW > -1.0 && initW < 2.5, "Initialization of the CRF weights " + initW + " is out of range")
	require( maxIters > 10 && maxIters < 250, "Maximum number of iterations for CRF training " + maxIters + " is out of range")
	require( lambda >= 0.0 && lambda <= 1.5, "The factor for the L2 penalty for CRF" + lambda + " is out of range")
	require( eps > 1e-5 && eps<= 0.1, "The convergence criteria for the CRF training " + eps + " is out of range")
	
		// textual description of the CRF configuration
	val params = new StringBuilder().append("initValue ").append(String.valueOf(initW))
		           .append(" maxIters ").append(String.valueOf(maxIters)).append(" lambda ")
		              .append(String.valueOf(lambda)).append( " scale ")
		                 .append(" true" ).append(" epsForConvergence ").append(String.valueOf(eps) )
		                   .append(" debugLvl ").append(debug).toString
}



	/**
	 * <p>Generic class for the linear chained CRF for tagging words, N-Grams or regular expression. The class
	 * define a Feature generator class that inherits the default implementation FeatureGenImpl of iitb features generator.
	 * The class assumes the training sequences are loaded from file with *.raw and *.tagged extensions.
	 * @param nLabels Number of labels (or tags) used in tagging training sequences of observations.
	 * @param config minimalist set of configuration parameters used in the CRF
	 * @param delims delimiters used in extracting labels and data from the training files
	 * @param taggedObs identifier for the training data set. The training set of sequences is defined by the raw observations
	 * and its associated tagged file    taggedObs.raw and taggedObs.tagged files.
	 * @exception IllegalArgumentException if nLabels, config, delims and taggedObs are either undefined or out of range.
	 * @see org.scalaml.workflow.PipeOperator
	 * 
	 * @author Patrick Nicolas
	 * @data April 3, 2014
	 */
class CrfLinearChain(val nLabels: Int, val config: CrfConfig, val delims: CrfSeqDelimiter, val taggedObs: String) 
                                               extends PipeOperator[String, Double] with Supervised[String] {
	
  require(nLabels > 0 && nLabels < 1000, "Number of labels for generating tags for CRF " + nLabels + " is out of range")
  require(config != null, "Configuration of the linear Chain CRF is undefined")
  require(delims != null, "delimiters used in the CRF training files are undefined")

  class TaggingGenerator(nLabels: Int) extends FeatureGenImpl(new CompleteModel(nLabels) , nLabels, true)
	
  private[this] val features = new TaggingGenerator(nLabels)
  private[this] lazy val model = new CRF(nLabels, features, config.params)
  val weights = train(taggedObs)
  
  		/**
  		 * Main training method for the CRF
  		 */

  override def |> (obs: String): Option[Double] = {
  	 if(weights != null) {
	  	 val dataSeq =  new CrfRecommendation(nLabels, obs, delims.dObs)
	  	 Some(model.apply(dataSeq))
  	 }
  	 else
  		 None
  }
  
  override def validate(output: XTSeries[(Array[String], Int)], index: Int): Double = -1.0
  
  private[this] def train(taggedObs: String): Option[DblVector] = {  
  	 val seqIter = CrfSeqIter(nLabels: Int, taggedObs, delims)
  	 try {
	  	 features.train(seqIter)
	  	 Some(model.train(seqIter))
  	 }
  	 catch {
  		 case e: IOException => Console.println(e.toString); None
  		 case e: Exception  =>Console.println(e.toString); None
  	 }
  }
}


// ---------------------------- EOF ------------------------------------------------------