/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.supervised.svm

import org.scalaml.supervised.Model
import libsvm.svm_model

/**
 *  @author Patrick Nicolas
 *  @since April 19, 2014
 *  @note Scala for Machine Learning
 */
final case class SVMModel(val params: (svm_model, Double)) extends Model {
	
   override def toString: String = 
  	  new StringBuilder("SVMModel: ")
          .append(params._1.toString)
            .append("\nAccuracy: ")
               .append(params._2)
                 .toString
}

// -------------------------  EOF ------------------------------