/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.app.chap3



import org.scalaml.core.types.ScalaMl
import org.scalaml.core.XTSeries
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import YahooFinancials._
import org.scalaml.util.Display
import org.apache.log4j.Logger
import scala.util.{Try, Success, Failure}

		/**
		 * Singleton used to test the moving average algorithms
		 */
object MovingAveragesEval extends FilteringEval {
	import org.scalaml.filtering._
	import ScalaMl._

	val name: String = "MovingAveragesEval"
  	 
	private val logger = Logger.getLogger(name)
   
	override def run(args: Array[String]): Int = {
		Display.show(s"$name Evaluation moving averages", logger)
  	 
		val symbol = args(0)
		val p = args(1).toInt
		val p_2 = p >>1
		val w = Array.tabulate(p)(n => if( n == p_2) 1.0 else 1.0/(Math.abs(n -p_2)+1))
		val weights: DblVector = w map { _ / w.sum }
		ScalaMl.toString(weights, "Weights", false)
     
		val dataSource = DataSource("resources/data/chap3/" + symbol + ".csv", false)
		Try {
			val price = dataSource |> YahooFinancials.adjClose
			val sMvAve = SimpleMovingAverage[Double](p)  
			val wMvAve = WeightedMovingAverage[Double](weights)
			val eMvAve = ExpMovingAverage[Double](p)
	
			val dataSink = DataSink[Double]("output/chap3/mvaverage" + p.toString + ".csv")
			val results = price :: sMvAve.|>(price) :: 
									sMvAve.|>(price) :: 
									sMvAve.|>(price) :: 
									List[XTSeries[Double]]()

			dataSink |> results
			Display.show(s"$name Results of different moving average", logger)
		}
		match {
			case Success(n) => n
			case Failure(e) => Display.error(s"$name Computation of moving averages failed", logger, e)
		}
	}
}



// --------------------------------------  EOF -------------------------------