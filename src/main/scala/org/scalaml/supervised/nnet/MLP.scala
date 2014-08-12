/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.nnet

import org.scalaml.core.Types.ScalaMl._
import scala.util.Random
import org.scalaml.core.XTSeries
import org.scalaml.workflow.PipeOperator



		/**
		 * <p>Implementation of the Multi-layer Perceptron as a Feed-foward
		 * Neural Network. The class uses the standard pattern of machine
		 * learning algorithm:<br>
		 * Model are created through training during instantiation of the class<br>
		 * The classifier is implemented as a data transformation and extends the PipeOperator trait.Br>
		 * This MLP uses the online training strategy suitable for time series.</p>
		 * @param config configuration parameters class for the MLP
		 * @param xt time series of features in the training set
		 * @param labels labeled or target observations used for training
		 * @param objective Objective of the model (classification or regression)
		 * @throws IllegalArgumentException if the any of the class parameters is undefined
		 * 
		 * @author Patrick Nicolas
		 * @since May 8, 2-14
		 * @note Scala for Machine Learning
		 */
import XTSeries._
final protected class MLP[T <% Double](val config: MLPConfig, 
		                     val xt: XTSeries[Array[T]], 
		                     val labels: DblMatrix,
		                     val objective: MLP.MLPObjective) extends PipeOperator[Array[T], DblVector] {
   validate(config, xt, labels, objective)
   
   var converged = false
   val model: Option[MLPModel] = {
  	 try {
	  	val _model = new MLPModel(config, xt(0), labels(0).size)
	  	  	// Scaling or normalization factor for the Mean Squares error
	  	val errScale = 1.0/(labels(0).size*xt.size)
	  	
	  		// Apply the exit condition for this online training strategy
	  	converged = Range(0, config.numEpochs).find( _ => {
		   xt.toArray.zip(labels).foldLeft(0.0)( (s, xtlbl) => {
		  	  _model.trainEpoch(xtlbl._1)
		  	  s +  _model.mse(xtlbl._2, objective)
		   })*errScale < config.eps		  	 
	  	}) != None
	  	Some(_model)   
  	 }
  	 catch {
  		 case e: IllegalArgumentException => println(e.toString); None
  		 case e: IllegalStateException =>  println(e.toString); None
  	 }
   }
   
   	/**
   	 * <p>Define the predictive function of the classifier or regression as a data
   	 * transformation by overriding the pipe operator |>.</p>
   	 * @param feature new data point that need to be either classified or regressed.
   	 * @return The normalized output of the neural network if model exists, None otherwise
   	 */
   override def |> (feature: Array[T]): Option[DblVector] = model match {
  	 case Some(_model) => {
  		require(feature == null || feature.size != dimension(xt), "Incorrect argument for MLP.|> method")
  		try {
  			Some(objective.normalize(_model.getOutput(feature)))
  		}
  		catch {
  			case e: IllegalStateException => None
  			case _: RuntimeException => None
  		}
  	  }
  	  case None => None
  }
   
   		/**
   		 * <p>Computes the accuracy of the training session. The accuracy is estimated
   		 * as the percentage of the training data points for which the square root of 
   		 * the sum of squares error, normalized by the size of the  training set exceed a predefined threshold.</p>
   		 * @param threshold threshold applied to the square root of the sum of squares error to validate the data point
   		 * @return accuracy value [0, 1] if model exits, None otherwise
   		 */
  def accuracy(threshold: Double): Option[Double] = {
  	 if( model != None ) {
  		 	// counts the number of data points for which the 
  		 val correct = xt.toArray.zip(labels).foldLeft(0)((s, xtl) =>  {
  			 val predicted = objective.normalize(model.get.getOutput(xtl._1))
  			 val diff = xtl._2.zip(predicted).foldLeft(0.0)((err,tp) => err + (tp._1 - tp._2)*(tp._1 - tp._2))
  			
  			 if( Math.sqrt(diff)/predicted.size < threshold)
  				s + 1
  		     else s
  		 })
  		 Some(correct.toDouble/xt.size)  // normalization
  	 }
  	 else None
  }

  
  private def validate(config: MLPConfig, xt: XTSeries[Array[T]], labels: DblMatrix, objective: MLP.MLPObjective): Unit = {
	   require(config != null, "Cannot train a multilayer perceptron without configuration parameters")
	   require(xt != null && xt.size > 0, "Features for the MLP are undefined")
	   require(labels != null && labels.size > 0, "Labeled observations for the MLP are undefined")
	   require(xt.size == labels.size, "Number of features for MLP " + xt.size + " is different from number of labels " + labels.size)
	   require(objective != null, "Objective for MLP is undefined")
   }
}


		/**
		 * <p>Companion object for the Multi-layer Perceptron. The singleton is used to:<br>
		 * Define several variants of the constructor<br>
		 * Define the class/trait hierarchy for the objective of the MLP {classification, regression}</p> 
		 */
object MLP {
	final val EPS = 1e-5
	
		/**
		 * <p>Trait that defined the signature of the objective function.<br>
		 * += for updating parameters if needed<br>
		 * normalize to normalize the output.</p>
		 */
	trait MLPObjective {	
		/**
		 * <p>+= operator to update the parameters of the objective
		 * function during training.The method is overriding for the classification only.
		 * The trait is defined to implement the regression by default.<p>
		 * @param z: new output value 
		 * @param index: index of the output value in the output vector
		 */
	   def += (z: Double, index: Int): Unit = {}
	   
	   	/**
	   	 * <p>Normalize the output vector to match the objective of the MLP. The
	   	 * output vector is the output layers minus the bias, output(0).</p>
	   	 * @param output raw output vector
	   	 * @return normalized output vector
	   	 */
	   def normalize(output: DblVector): DblVector = output.drop(1)
	}
	
		/**
		 * Class signature for the Regression objective for the MLP
		 */
	class MLPRegression extends MLPObjective
	
		/**
		 * <p>Class signature for the classification objective. The class
		 * implements a normalization to convert the output vector into
		 * a probability [0, 1].</p>
		 * @param labels labeled or target observations used in the training set
		 */
	class MLPClassification(val labels: DblMatrix) extends MLPObjective {
	  private val min = Array.fill(labels(0).size)(Double.MaxValue)
	  private val max = Array.fill(labels(0).size)(-Double.MaxValue) 
	  
	  	
	  	/**
		 * <p>Overloaded += operator to update the parameters of the classification.<p>
		 * @param z: new output value 
		 * @param index: index of the output value in the output vector
		 */
	  override def += (z: Double, index: Int): Unit = {
	  	if(z < min(index))
  	  	   min(index) = z
  	  	if(z >  max(index))
  	  	   max(index) = z
	  }
	  	   	
	    /**
	   	 * <p>Normalize the output vector with the minimum and maximum values
	   	 * of each output values during training.</p>
	   	 * @param output raw output vector
	   	 * @return normalized output vector
	   	 */

      override def normalize(output: DblVector): DblVector = 
      	 output.drop(1)
      	    .zipWithIndex
      	    .map(o => (o._1 - min(o._2))/(max(o._2) - min(o._2)))
	}
	
	def apply[T <% Double](config: MLPConfig, xt: XTSeries[Array[T]], labels: DblMatrix, objective: MLPObjective): MLP[T] = 
		    new MLP[T](config, xt, labels, objective)
		    
    def apply[T <% Double](config: MLPConfig, features: Array[Array[T]], labels: DblMatrix, objective: MLPObjective): MLP[T] =
            new MLP[T](config, XTSeries[Array[T]](features), labels, objective)
    
    def apply[T <% Double](config: MLPConfig, features: Array[Array[T]], labels: DblVector, objective: MLPObjective): MLP[T] =
            new MLP[T](config, XTSeries[Array[T]](features), Array[DblVector](labels), objective)
}



// ----------------------------------------------  EOF ------------------------------------