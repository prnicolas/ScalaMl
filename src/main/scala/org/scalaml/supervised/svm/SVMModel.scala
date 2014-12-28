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
 * Version 0.98
 */
package org.scalaml.supervised.svm

import libsvm.svm_model

import org.scalaml.core.Design.Model
import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.Types.ScalaMl.DblMatrix
import org.scalaml.util.FormatUtils


		/**
		 * <p>Defined a model for support vector machine. The model is composed of the svm_model
		 * parameters of <b>LIBSVM</b> library and the accuracy computed during training.<br>
		 * This implementation uses the LIBSVM library: http://www.csie.ntu.edu.tw/~cjlin/libsvm/</p>
		 * @constructor Create a SVMModel with a given LIBSVM model and accuracy
		 * @see LIBSVM
		 * @see org.scalaml.core.Design.Model
		 * @throws IllegalArgumentExceptin if LIBSVM model is undefined.
		 * @param svmmodel Model parameters as defined in <b>LIBSVM</b>
		 * @param accuracy	Accuracy of the model from the training process
		 * 
		 * @author Patrick Nicolas
		 * @since April 19, 2014
		 * @note Scala for Machine Learning  Chapter 8 Kernel models and support vector machines
		 */
final protected class SVMModel(val svmmodel: svm_model, val accuracy: Double) extends Model {
	require(svmmodel != null, "SVMModel LIBSVM model smmodel is undefined")
 
		/**
		 * Textual representation of the SVM model. The method converts LIBSVM nodes values
		 * and the SVM weights (or coefficients) into characters string
		 * @return Description of the model for debugging purpose
		 */
	override def toString: String = {
		val description = new StringBuilder("SVM model\n")
			// Stringize LIBSVM nodes
		val nodes = svmmodel.SV
		if( !nodes.isEmpty) {
			description.append("SVM nodes:\n")
			val _nodes: DblMatrix = Array.tabulate(nodes.size)(n => {
				val row = nodes(n)
				row.map(r => r.value)
			})
			description.append(FormatUtils.format(_nodes, FormatUtils.ShortFormat))
		}
  		// Stringize the LIBSVM coefficients
		val coefs = svmmodel.sv_coef
		if( !coefs.isEmpty ) {
			description.append("\nSVM basis functions:\n")
			coefs.foreach(w => {
				w.foreach(c => description.append(s"${FormatUtils.format(c,"",FormatUtils.ShortFormat)}\n"))
				description.append("\n")
			})
		}
			// Aggregate the description
		description.append(s"Accuracy: ${FormatUtils.format(accuracy,"",FormatUtils.ShortFormat)}\n")
						.toString
	}
}


// -------------------------  EOF ------------------------------