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
package org.scalaml.app.chap12


import java.lang.ref._
import scala.math._

import org.apache.log4j.Logger
	
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.app.Eval
import org.scalaml.util.DisplayUtils._

case class DataPoint(x: DblVector, y: Double)


class LossFunction(
    f: (DblVector, DblVector) => Double, weights: DblVector, dataSize: Int) {
	
	var nElements = 0
	private val logger = Logger.getLogger("LossFunction")
	private val STEP = dataSize/10
	
	def compute(stream: () => WeakReference[Stream[DataPoint]]): Double = compute(stream().get, 0.0)
		
	@scala.annotation.tailrec
	private def compute(stream: Stream[DataPoint], loss: Double): Double = {
		if( nElements >= dataSize)
			loss
		else {
		  val step = if( nElements + STEP > dataSize)  dataSize - nElements else STEP
		  nElements += step
			val newLoss = _loss(stream.take(step).toList)
			compute(stream.drop(STEP), loss + newLoss)
		}
	}
			
	def _loss(xs: List[DataPoint]): Double = xs.map(
		dp => {
		  val z = dp.y - f(weights, dp.x)
		  z
		}
	).map( sqr(_)).sum
}

	/**
	 * '''Purpose''': Singleton to illustrate Scala streams
	 * 
	 * @author Patrick Nicolas 
	 * @see Scala for Machine Learning Chapter 12 Scalable frameworks/ Scala streams
	 */
object StreamsEval extends Eval {
	 import scala.util.Random

	 
	 	/**
		 * Name of the evaluation 
		 */
	val name: String = "StreamsEval"
	final val DATASIZE = 20000
	
	override protected def run(args: Array[String]): Int = {
		show(s"$header Scala streams evaluation")
		
		val diff = (x: DblVector, y: DblVector) => x.zip(y).aggregate(0.0)( (s, xy) => s +  xy._1*xy._2, _ + _) 
		val weights = Vector[Double](0.5, 0.7)
		val lossFunction = new LossFunction( diff, weights , DATASIZE)
		
		  // Create a stream of weak references to 10 stream segments of size DATESIZE/10
		val stream = () => new WeakReference( 
		      Stream.tabulate(DATASIZE)(n => 
		          DataPoint( Vector[Double](n.toDouble, n*(n.toDouble)), 
		                      n.toDouble*weights(0) + n*(n.toDouble)*weights(1) + 0.1*Random.nextDouble)
		      )
		    )
		   // Compute a simple distance using the dot product 
		val totalLoss = sqrt( lossFunction.compute(stream) )
		
		show(s"Total loss expected and computed value $totalLoss\nAverage loss ${totalLoss/DATASIZE}")
	}
}

// --------------------------  EOF --------------------------------