/**
 */
package org.scalaml.supervised.svm

import org.scalaml.supervised.Model
import libsvm.svm_model

/**
 *  @author Patrick Nicolas
 *  @since Jul 19, 2014
 *  @note Book
 */
class SVMModel(val params: (svm_model, Double)) extends Model {
   
}

// -------------------------  EOF ------------------------------