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
package org.scalaml.supervised.svm

import libsvm.svm_model

import org.scalaml.core.Design.Model
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.Types.emptyString
import org.scalaml.core.Types.ScalaMl.DblMatrix
import org.scalaml.util.FormatUtils._


		/**
		 * Defined a model for support vector machine. The model is composed of the svm_model
		 * parameters of '''LIBSVM''' library and the accuracy computed during training.
		 * 
		 * This implementation uses the LIBSVM library: ''http://www.csie.ntu.edu.tw/~cjlin/libsvm/''
		 * @constructor Create a SVMModel with a given LIBSVM model and accuracy
		 * @see LIBSVM
		 * @see org.scalaml.core.Design.Model
		 * @throws IllegalArgumentException if LIBSVM model is undefined.
		 * @param svmmodel Model parameters as defined in '''LIBSVM'''
		 * @param accuracy	Accuracy of the model from the training process
		 * 
		 * @author Patrick Nicolas
		 * @since April 19, 2014
		 * @note Scala for Machine Learning  Chapter 8 Kernel models and support vector machines
		 */
final class SVMModel(val svmmodel: svm_model, val accuracy: Double) extends Model {
	require(svmmodel != null, "SVMModel LIBSVM model smmodel is undefined")
	
	lazy val residuals: DblArray = svmmodel.sv_coef(0)
 
		/**
		 * Textual representation of the SVM model. The method converts LIBSVM nodes values
		 * and the SVM weights (or coefficients) into characters string
		 * @return Description of the model for debugging purpose
		 */
	override def toString: String = {
		val description = new StringBuilder("SVM model\n")
		
			// Stringize LIBSVM nodes
		val nodes = svmmodel.SV
		if( nodes.nonEmpty) {
			description.append("SVM nodes:\n")
			val _nodes: DblMatrix = Array.tabulate(nodes.size)(n => {
				val row = nodes(n)
				row.map(r => r.value)
			})
			description.append(format(_nodes, SHORT))
		}
  		// Stringize the LIBSVM coefficients
		val coefs = svmmodel.sv_coef
		if( coefs.nonEmpty ) {
			description.append("\nSVM basis functions:\n")
			coefs.foreach(w => {
				w.foreach(c => description.append(s"${format(c, emptyString, SHORT)}\n"))
				description.append("\n")
			})
		}
			// Aggregate the description
		description.append(s"Accuracy: ${format(accuracy, emptyString, SHORT)}\n")
			.toString()
	}
}


// -------------------------  EOF ------------------------------