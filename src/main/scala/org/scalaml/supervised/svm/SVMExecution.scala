/**
 */
package org.scalaml.supervised.svm

import libsvm._


class SVMExecution(val cacheSize: Int, val eps: Double, val nFolds: Int) extends SVMConfigItem {
	override def config(param: svm_parameter): Unit = {
		param.cache_size = cacheSize
        param.eps = eps
	}

	override def toString: String = new StringBuilder("\nCache size: ").append(cacheSize).append("\neps: ").append(eps).toString
}

object SVMExecution {
	final val DEFAULT_CACHE_SIZE=2000
	def apply(cacheSize: Int, eps: Double, nFolds: Int): SVMExecution = new SVMExecution(cacheSize, eps,nFolds)
    def apply(eps: Double, nFolds: Int): SVMExecution = new SVMExecution(DEFAULT_CACHE_SIZE, eps, nFolds)
	def apply(eps: Double): SVMExecution = new SVMExecution(DEFAULT_CACHE_SIZE, eps, -1)
}


// --------------------------- EOF ------------------------------------------