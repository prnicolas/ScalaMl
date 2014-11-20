/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.supervised.nnet

import org.scalaml.core.types.ScalaMl._
import scala.util.{Random, Try, Success, Failure}
import org.scalaml.core.XTSeries
import org.scalaml.core.design.PipeOperator
import XTSeries._
import org.apache.log4j.Logger
import org.scalaml.util.Display



		/**
		 * <p>Implementation of the Multi-layer Perceptron as a Feed-foward
		 * Neural Network. The class uses the standard pattern of machine
		 * learning algorithm:<br>
		 * Model are created through training during instantiation of the class<br>
		 * The classifier is implemented as a data transformation and extends the PipeOperator trait.<br>
		 * This MLP uses the online training strategy suitable for time series.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * <b>config</b>     Configuration parameters class for the MLP
		 * <b>xt</b>         Time series of features in the training set
		 * <b>labels</b>     Labeled or target observations used for training
		 * <b>objective</b>  Objective of the model (classification or regression)
		 * </span></pre></p>
		 * @constructor Instantiates a Multi-layer Perceptron for a specific configuration, time series and target or labeled data. 
		 * @throws IllegalArgumentException if the any of the class parameters is undefined
		 * 
		 * @author Patrick Nicolas
		 * @since May 8, 2014
		 * @note Scala for Machine Learning Chapter 9 Artificial Neural Network/Multilayer perceptron/Training cycle/epoch
		 */
final protected class MLP[T <% Double](config: MLPConfig, xt: XTSeries[Array[T]], labels: DblMatrix)
				(implicit mlpObjective: MLP.MLPObjective) extends PipeOperator[Array[T], DblVector] {
	import MLP._
	
	check(config, xt, labels)
	private val logger = Logger.getLogger("MLP")
   
	var converged = false
	val model: Option[MLPModel] = {
		Try {
			val _model = new MLPModel(config, xt(0).size, labels(0).size)(mlpObjective)
				// Scaling or normalization factor for the sum of the squared error
			val errScale = 1.0/(labels(0).size*xt.size)
	  	
				// Apply the exit condition for this online training strategy
			converged = Range(0, config.numEpochs).find( _ => {
				xt.toArray.zip(labels).foldLeft(0.0)( (s, xtlbl) => 
					s + _model.trainEpoch(xtlbl._1, xtlbl._2)
				)*errScale < config.eps		  	 
			}) != None
			_model
		}
		match {
			case Success(_model) => Some(_model)
			case Failure(e) => Display.none("MLP.model ", logger, e)
		}
	}
   
   	
		/**
		 * <p>Define the predictive function of the classifier or regression as a data
		 * transformation by overriding the pipe operator |>.</p>
		 * @throws MatchError if the model is undefined or the input string has an incorrect size
		 * @return PartialFunction of features vector of type Array[T] as input and the predicted vector values as output
		 */
	override def |> : PartialFunction[Array[T], DblVector] = {
		case x: Array[T] if(x != null && model != None && x.size == dimension(xt)) => {
			Try(model.get.getOutput(x)) match {
				case Success(y) => y
				case Failure(e) => Display.error("MLP.|> ", logger, e); Array.empty
			}
		}
	}
   

		/**
		 * <p>Computes the accuracy of the training session. The accuracy is estimated
		 * as the percentage of the training data points for which the square root of 
		 * the sum of squares error, normalized by the size of the  training set exceed a predefined threshold.</p>
		 * @param threshold threshold applied to the square root of the sum of squares error to validate the data point
		 * @return accuracy value [0, 1] if model exits, None otherwise
		 */
	final def accuracy(threshold: Double): Option[Double] = {
		if( model != None ) {
				// counts the number of data points for which the 
			val correct = xt.toArray.zip(labels).foldLeft(0)((s, xtl) =>  {
				
				val output = model.get.getOutput(xtl._1)
				val _sse = xtl._2.zip(output.drop(1)).foldLeft(0.0)((err,tp) => { 
					val diff= tp._1 - tp._2
					err + diff*diff
				})*0.5
  			 
				val error = Math.sqrt(_sse)/(output.size -1)
				if( error < threshold)
					s + 1
				else s
			})
			Some(correct.toDouble/xt.size)  // normalization
		}
		else 
			Display.none("MLP.accuracy ", logger)
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
		 * <p>Normalize the output vector to match the objective of the MLP. The
		 * output vector is the output layers minus the bias, output(0).</p>
		 * @param output raw output vector
		 * @return normalized output vector
		 */
		def apply(output: DblVector): DblVector
	}
	
		/**
		 * <p>Class for the binary classification objective using the Multi-layer perceptron.
		 */
	class MLPBinClassifier extends MLPObjective {
		
		/**
		 * <p>Normalize the output vector to match the objective of the MLP. The
		 * output vector is the output layers minus the bias, output(0).</p>
		 * @param output raw output vector
		 * @return normalized output vector
		 */
		override def apply(output: DblVector): DblVector = output
	}
	
		/**
		 * Class signature for the Regression objective for the MLP
		 */
	class MLPRegression extends MLPObjective  {
		
		/**
		 * <p>Normalize the output vector to match the objective of the MLP. The
		 * output vector is the output layers minus the bias, output(0).</p>
		 * @param output raw output vector
		 * @return normalized output vector
		 */
		override def apply(output: DblVector): DblVector = output
	}
	
	
		/**
		 * Class for the Regression objective for the MLP. This implementation uses softmax 
		 */
	class MLPMultiClassifier extends MLPObjective {
				
		/**
		 * <p>Normalize the output vector to match the objective of the MLP. The
		 * output vector is the output layers minus the bias, output(0).</p>
		 * @param output raw output vector
		 * @return normalized output vector
		 */
		override def apply(output: DblVector): DblVector =  softmax(output.drop(1))
		
		private def softmax(y: DblVector): DblVector = {
			val softmaxValues = new DblVector(y.size)
			val expY = y.map( Math.exp(_))
			val expYSum = expY.sum
			expY.map( _ /expYSum).copyToArray(softmaxValues , 1)
			softmaxValues
		}
	}
	
	
	def apply[T <% Double](config: MLPConfig, xt: XTSeries[Array[T]], labels: DblMatrix)(implicit mlpObjective: MLP.MLPObjective): MLP[T] = 
		new MLP[T](config, xt, labels)
		    
	def apply[T <% Double](config: MLPConfig, features: Array[Array[T]], labels: DblMatrix)(implicit mlpObjective: MLP.MLPObjective): MLP[T] =
		new MLP[T](config, XTSeries[Array[T]](features), labels)
    
	def apply[T <% Double](config: MLPConfig, features: Array[Array[T]], labels: DblVector)(implicit mlpObjective: MLP.MLPObjective): MLP[T] =
		new MLP[T](config, XTSeries[Array[T]](features), labels.map(Array[Double](_)))
            
            
	private def check[T](config: MLPConfig, xt: XTSeries[Array[T]], labels: DblMatrix): Unit = {
		require(config != null, "Cannot train a multilayer perceptron without stateuration parameters")
		require(xt != null && xt.size > 0, "Features for the MLP are undefined")
		require(labels != null && labels.size > 0, "Labeled observations for the MLP are undefined")
		require(xt.size == labels.size, s"Number of features for MLP ${xt.size} is different from number of labels ${labels.size}")
	}
}


// ----------------------------------------------  EOF ------------------------------------