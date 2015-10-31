package org.scalaml.app.chap12


import java.lang.ref._
import org.scalaml.core.Types.ScalaMl._

case class DataPoint(x: DblVector, y: Double)


class LossFunction(
    f: (DblVector, DblVector) => Double, weights: DblVector, dataSize: Int) {
	import StreamsEval._
	
	var nElements = 0
		
	def compute(stream: () => WeakReference[Stream[DataPoint]]): Double = compute(stream().get, 0.0)
		
	@scala.annotation.tailrec
	private def compute(stream: Stream[DataPoint], loss: Double): Double = {
		if( nElements >= dataSize)
			loss
		else {
		  val step = if( nElements + STEP > dataSize)  - nElements else STEP
		  nElements += step
			val newLoss = _loss(stream.take(step).toList)
			compute(stream.drop(STEP), loss + newLoss)
		}
	}
			
	def _loss(xs: List[DataPoint]): Double = xs.map(
		dp => dp.y - f(weights, dp.x)
	).map( sqr(_)).sum
}




object StreamsEval {
	val NUM_ELEMENTS = 1000000
	val STEP = NUM_ELEMENTS/10

}


// --------------------------  EOF --------------------------------