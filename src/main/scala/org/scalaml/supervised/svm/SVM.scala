/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.2
 */
package org.scalaml.supervised.svm

import org.scalaml.core.XTSeries
import libsvm.{svm_problem, svm_node, svm, svm_model}
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.core.design.PipeOperator
import org.scalaml.core.Matrix
import scala.util.{Try, Success, Failure}
import XTSeries._
import org.apache.log4j.Logger
import org.scalaml.util.DisplayUtils
import scala.collection.mutable.ArrayBuffer



		/**
		 * <p>Support Vector Algorithm for time series of vector of element with parameterized types
		 * The model is created or trained during the instantiation of the class. Therefore a 
		 * model either is properly trained or does not exists.<br>
		 * This implementation uses the LIBSVM library: http://www.csie.ntu.edu.tw/~cjlin/libsvm/</p>
		 * @constructor Create a SVM algorithm for a labeled time series given a configuration.
		 * @see LIBSVM 	
		 * @throws IllegalArgumentException if the configuration or the time series is undefined.
		 * @param configConfiguration of this SVM
		 * @param xt Time series to regress or classify
		 * @param labels Labeled values for the time series used in the training of the SVM.
		 * @author Patrick Nicolas
		 * @since April 28, 2014
		 * @note Scala for Machine Learning Chapter 8 Kernel models and support vector machines.
		 */
final class SVM[T <% Double](config: SVMConfig, xt: XTSeries[Array[T]], labels: DblVector) 
						extends PipeOperator[Array[T], Double] {
	import SVM._
	
	check(config, xt, labels)
  
	type Feature = Array[T]
	type SVMNodes = Array[Array[svm_node]]

	private val logger = Logger.getLogger("SVM")
	private val normEPS = config.eps*1e-7
    
	private[this] val model: Option[SVMModel] = {
		val problem = new svm_problem
		problem.l = xt.size;
		problem.y = labels  
		problem.x = new SVMNodes(xt.size)
      
		val dim = dimension(xt)
		Try {
			xt.zipWithIndex.foreach( xt_i => {  
	  		 
				val svm_col = new Array[svm_node](dim)
				xt_i._1.zipWithIndex.foreach(xi =>  {
					val node = new svm_node
					node.index= xi._2
					node.value = xi._1
					svm_col(xi._2) = node 
				})
	  	  	 
				problem.x(xt_i._2) = svm_col
			})
			new SVMModel(svm.svm_train(problem, config.param), accuracy(problem))
		} 
		match {
			case Success(m) => Some(m)
			case Failure(e) => DisplayUtils.none("SVM.model", logger, e)
		}
	}
  
  
		/**
		 * Access the accuracy of the SVM algorithm. 
		 * @return accuracy value in the range [0, 1] if the model was successfully trained, None otherwise
		 */
	final def accuracy: Option[Double] = if( model != None) Some(model.get.accuracy) else None

		/**
		 * Method to compute the Means Square Error for the training of the SVM
		 * @return Mean square error as square root of the sum of the square errors, if model was 
		 * successfully built, None otherwise
		 */
	final def mse: Option[Double] = model match {
		case Some(m) => {
			val z = xt.toArray.zipWithIndex.foldLeft(0.0)((s, xti) => {
				val diff = svm.svm_predict(m.svmmodel, toNodes(xti._1)) - labels(xti._2)
						s + diff*diff
			})
			Some(Math.sqrt(z)/xt.size)
		}
		case None =>  DisplayUtils.none("SVM.mse model is undefined", logger)
	}
  

		/**
		 * Compute the margin 2/||w|| for the SVM model (distance between the support vectors)
		 * @return margin if model was successfully trained, None otherwise of if the model norm is zero
		 */
	def margin: Option[Double] = model match {
		case Some(m) => {	
			val wNorm = m.svmmodel.sv_coef(0).foldLeft(0.0)((s, r) => s + r*r)
			if(wNorm < normEPS)
				DisplayUtils.none(s"SVM.margin sum of squared errors $wNorm is too small", logger)
			else
				Some(2.0/Math.sqrt(wNorm))
		}
		case None => DisplayUtils.none("SVM.margin model is undefined", logger)
	}
  

		/**
		 * <p>Data transformation that implements the prediction value using SVM</p>
		 * 	@throws MatchError if the model is undefined or has an incorrect size or the input feature 
		 *  is undefined
		 *  @return PartialFunction of feature of type Array[T] as input and the predicted value as output
		 */
	override def |> : PartialFunction[Feature, Double] =  {
		case x: Feature if(!x.isEmpty && x.size == dimension(xt) && model != None && 
				model.get.accuracy >= 0.0) =>
			svm.svm_predict(model.get.svmmodel, toNodes(x))
	}


	private def accuracy(problem: svm_problem): Double = config.isCrossValidation match { 
		case true => {
			val target = new Array[Double](labels.size)
			svm.svm_cross_validation(problem, config.param, config.nFolds, target)
			target.zip(labels).filter(z => Math.abs(z._1-z._2) < config.eps).size.toDouble/labels.size
		}
		case false => 0.0
	}
  
  
	private def toNodes(x: Feature): Array[svm_node] = 
		x.zipWithIndex.foldLeft(new ArrayBuffer[svm_node])((xs, f) =>  {
			val node = new svm_node
			node.index = f._2
			node.value = f._1
			xs.append(node)
			xs
		}).toArray

	override def toString: String =
		if(model != None) 
			s"${config.toString}\n${model.get.toString}" 
		else "SVM model undefined"
}


		/**
		 * Companion object for the SVM algorithms. It is used to defined the constructors for 
		 * the parameterized class SVM
		 * @author Patrick Nicolas
		 * @since April 28, 2014
		 * @note Scala for Machine Learning Chapter 8 Kernel models and support vector machines.
		 */
object SVM {

		/**
		 * Default constructor for the support vector machine
		 * @param configConfiguration of this SVM
		 * @param xt Time series to regress or classify
		 * @param labels Labeled values for the time series used in the training of the SVM.
		 */
	def apply[T <% Double](config: SVMConfig, xt: XTSeries[Array[T]], labels: DblVector): SVM[T] = 
		new SVM[T](config, xt, labels)

		/**
		 * Constructor for the support vector machine that uses an array of features
		 * as observations instead of a time series
		 * @param configConfiguration of this SVM
		 * @param ft Array of features used in the regression
		 * @param labels Labeled values for the time series used in the training of the SVM.
		 */
	def apply[T <% Double](config: SVMConfig, ft: Array[Array[T]], labels: DblVector): SVM[T] = 
		new SVM[T](config, XTSeries[Array[T]](ft), labels)
		
	private def check[T <% Double](state: SVMConfig, xt: XTSeries[Array[T]], labels: DblVector) {
		require( !xt.isEmpty, "SVM.check  Features for the SVM are undefined")
		require( !labels.isEmpty, 
				"SVM.check  Labeled observations for the SVM are undefined")
		require(xt.size == labels.size, 
				s"SVM.check  Number of features ${xt.size} != number of labels ${labels.size}")
	}
}

// ----------------------------------  EOF ------------------------------------