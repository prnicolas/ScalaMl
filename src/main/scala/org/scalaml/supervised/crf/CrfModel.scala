/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98.1
 * 
 * This code uses the iitb CRF library http://sourceforge.net/projects/crf/
 * Copyright (c) <2004> <Sunita Sarawagi Indian Institute of Technology Bombay> All rights reserved.
 */
package org.scalaml.supervised.crf

import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.core.Design.Model

		/**
		 * <p>Generic model for Conditional Random fields. The model consists merely of the CRF weights.</p>
		 * @constructor Instantiate a model for CRF after training is completed.
		 * @throws IllegalArgumentException if weights is not properly defined
		 * @param weights	Weights (or lambda parameters) for this CRF model.
		 * @see org.scalaml.core.Design.Model
		 * 
		 * @author Patrick Nicolas
		 * @since April 1, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Conditional Random Fields.
		 */
final protected class CrfModel(val weights: DblVector) extends Model {
	require( !weights.isEmpty, "CrfModel Cannot create a model with undefined weights")

		/**
		 * Constructor that load the model from file "model/CrfModel"
		 * @param className name of the class of the model, which is also the name of the file
		 * @return instance of the CrfModel with appropriate weights if the model has been 
		 * saved into file
		 * @note The client code that instantiate the model is responsible for catching the 
		 * exception if the file does not exist.
		 */
	def this(className: String) = 
			this({ Model.read(className).map( _.split(",").map(_.toDouble)).getOrElse(Array.empty) })
	
			/**
			 * Write the content of this model (weights) into a file
			 * @return true if the model/weights were saved into file, false otherwise.
			 */
	override def >> : Boolean = write(weights.mkString(","))
}



// ---------------------------- EOF ------------------------------------------------------