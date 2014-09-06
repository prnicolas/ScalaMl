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
package org.scalaml.supervised.svm

import org.scalaml.core.XTSeries
import libsvm.{svm_problem, svm_node, svm, svm_model}
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.workflow.PipeOperator
import org.scalaml.util.Matrix
import scala.util.{Try, Success, Failure}
import XTSeries._
import org.apache.log4j.Logger
import org.scalaml.util.Display




final class SVM[T <% Double](val config: SVMConfig, 
		               val xt: XTSeries[Array[T]], 
		               val labels: DblVector) extends PipeOperator[Array[T], Double] {
	
  private val logger = Logger.getLogger("SVM")
  validate(config, xt, labels)
  
  type Feature = Array[T]
  type SVMNodes = Array[Array[svm_node]]
  
  private val model: Option[SVMModel] = {
  	 
  	  val prob = new svm_problem
      prob.l = xt.size;
      prob.y = labels  
      prob.x = new SVMNodes(xt.size)
      
      val dim = dimension(xt)
      Try {
	  	  xt.zipWithIndex.foreach( xt_i => {  
	  		 
	  		 val svm_col = new Array[svm_node](dim)
	         xt_i._1.zipWithIndex.foreach(xi =>  {
	  	  	    val node = new svm_node
	  	  	    node.index= xi._2
	  	  	    node.value = xi._1
	  	  	    svm_col(xi._2) = node })
	  	  	 prob.x(xt_i._2) = svm_col
	      })
	      SVMModel(svm.svm_train(prob, config.param), accuracy(prob))
      } match {
      	case Success(m) => Some(m)
      	case Failure(e) => Display.error("SVM.model ", logger, e); None
      }
  }
  
  
  	/**
  	 * Access the accuracy of the SVM algorithm. 
  	 * @return accuracy value in the range [0, 1] if the model was successfully trained, None otherwise
  	 */
  final def accuracy: Option[Double] = if( model != None) Some(model.get.params._2) else None

  
	/**
	 * Method to compute the Means Square Error for the training of the SVM
	 * @return Mean square error as square root of the sum of the square errors, if model was successfully built, None otherwise
	 */
  final def mse: Option[Double] = model match {
  	case Some(m) => {
  		val z: Double = xt.toArray.zipWithIndex
  		          .foldLeft(0.0)((s, xti) => {
  		          	val diff = svm.svm_predict(m.params._1, featurePrediction(xti._1)) - labels(xti._2)
  		          	s + diff*diff
  		          })
        Some(Math.sqrt(z))
  	}
  	case None => None
  }
  
  	/**
  	 * Compute the margin 2/||w|| for the SVM model (distance between the support vectors)
  	 * @return margin if model was successfully trained, None otherwise of if the model norm is zero
  	 */
  def margin: Option[Double] = model match {
  	case Some(m) => {
  		val wNorm = m.params._1.sv_coef(0).foldLeft(0.0)((s, r) => s + r*r)
  		if(wNorm < config.eps)
  			None
  		else
  			Some(2.0/Math.sqrt(wNorm))
     }
  	case None => None
  }
  
  	/**
  	 * Data transformation that implements the prediction value using SVM
  	 * @param x feature used as input to the prediction
  	 * @throws IllegalStateException if the features vector is undefined or have an incorrect size
  	 */
  override def |> (x: Feature): Option[Double] = model match {
  	case Some(m) => {
  	   if(x == null || x.size != dimension(xt))
  			throw new IllegalStateException("Size of input data for prediction " + x.size + " should be " + dimension(xt))
  		Some(svm.svm_predict(m.params._1, featurePrediction(x)))
  	}
  	case None => None
  }


  private def accuracy(prob: svm_problem): Double = {
	 if( config.isCrossValidation ) { 
	    val target = new Array[Double](labels.size)
	    svm.svm_cross_validation(prob, config.param, 5, target)
	  	target.zip(labels).filter(z => {println(z._1.toString + ", " + z._2.toString); Math.abs(z._1-z._2) < 1e-3}).size.toDouble/labels.size
	 }
	 else -1.0
  }
  
  
  private def featurePrediction(x: Feature): Array[svm_node] = 
  	  x.zipWithIndex.foldLeft(List[svm_node]())((xs, f) =>  {
  			val node = new svm_node
  			node.index = f._2
  			node.value = f._1
  			node :: xs
  	  }).toArray.reverse

  
  private def validate(config: SVMConfig, xt: XTSeries[Array[T]], labels: DblVector) {
	  require(config != null, "Configuration of the SVM is undefined")
	  require(xt != null && xt.size > 0, "Features for the SVM are undefined")
	  require(labels != null && labels.size > 0, "Labeled observations for the SVM are undefined")
	  require(xt.size == labels.size, "Number of features " + xt.size + " and number of labels " + labels.size + " differs for SVM")
  }
  
  override def toString: String = 
  	 new StringBuilder(config.toString)
           .append("\n") 
              .append(model.get.toString)
                 .toString
}


		/**
		 * Companion object for the SVM algorithms. It is used to defined the constructors for 
		 * the parameterized class SVM
		 */
object SVM {
  
  def apply[T <% Double](config: SVMConfig, xt: XTSeries[Array[T]], labels: DblVector): SVM[T] = new SVM[T](config, xt, labels)
  def apply[T <% Double](config: SVMConfig, ft: Array[Array[T]], labels: DblVector): SVM[T] = new SVM[T](config, XTSeries[Array[T]](ft), labels)
}

// ----------------------------------  EOF ------------------------------------