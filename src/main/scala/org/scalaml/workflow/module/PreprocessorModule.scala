/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.workflow.module


import org.scalaml.workflow.Transform
import org.scalaml.stats.Stats
import org.scalaml.core.Types._
import scala.Array.canBuildFrom

/*
trait PreProcessorModule[T] {
   val preProcessor: PreProcessor[T]

    class PreProcessor[T](implicit fn: T => Array[DblVector]) extends Transform[T, Array[DblVector]](fn) { 
        def |> (data: T): Array[Array[Double]] =  fn(data).map( field => Stats[Double](field).normalize  )
    }
}
* 
*/



// ----------------------------------  EOF -------------------------------------