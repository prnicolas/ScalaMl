package org.scalaml.supervised.regression


import org.scalaml.core.Types.ScalaMl.DblVector


case class RegressionModel(val weights: DblVector, val accuracy: Double) {
   @inline def size: Int = weights.size
}

// ------------------------  EOF ----------------------------------------------------