/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99.1
 */
package org.scalaml.supervised.crf

import org.scalaml.core.Types.ScalaMl.DblArray
import org.scalaml.core.Design.Model

		/**
		 * Generic model for Conditional Random fields. The model consists merely of the CRF weights.
		 * 
		 * @constructor Instantiate a model for CRF after training is completed.
		 * @throws IllegalArgumentException if weights is not properly defined
		 * @param weights	Weights (or lambda parameters) for this CRF model.
		 * @see org.scalaml.core.Design.Model
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98 April 1, 2014
		 * @version 0.99.1
		 * @see Scala for Machine Learning Chapter 7 "Sequential data models"/Conditional Random Fields.
		 */
final protected class CrfModel(val weights: DblArray) extends Model {
	require( weights.length >0, "CrfModel Cannot create a model with undefined weights")

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