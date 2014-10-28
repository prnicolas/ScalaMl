/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.app.chap5


import org.scalaml.supervised.bayes._
import org.scalaml.core.{types, XTSeries}
import org.scalaml.workflow.data.{DataSource,DocumentsSource}
import scala.collection.mutable.ArrayBuffer
import types.ScalaMl._
import org.scalaml.filtering.SimpleMovingAverage
import SimpleMovingAverage._
import scala.collection.immutable.HashSet
import org.scalaml.supervised.bayes.NaiveBayes
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display


		/**
		 * <p>Generic trait that implements the extraction of data associated to a stock
		 * or ETF ticker symbol, located into a directory
		 */
trait BayesEval {
   import org.scalaml.trading.YahooFinancials._
   final val path = "resources/data/chap5/"
       
   def run(args: Array[String]): Int
  
   protected val extractor = toDouble(CLOSE) :: 
                             ratio (HIGH, LOW) :: 
                             toDouble(VOLUME) ::
                             List[Array[String] =>Double]()
   protected def symbolFiles = DataSource.listSymbolFiles(path)
}



			/**
			 * <p>Singleton to evaluate the Binomial Naive Bayes classifier.</p>
			 */
object BinomialBayesEval extends BayesEval {
	private val logger = Logger.getLogger("BinomialBayesEval")
	
	override def run(args: Array[String]): Int = {
	  require(args != null && args.size >2, "BinomialBayesEval.run incorrect arguments list")
	   Display.show("Evaluation Multinomial Naive Bayes", logger)	
		
	   val trainValidRatio = args(1).toDouble
	   val period = args(2).toInt
	   val description = s"symbol: ${args(0)} smoothing period:$period"
	   Try {
		   val input = load(args(0), period)
	       val labels = XTSeries[(Array[Int], Int)](input.map( x => (x._1.toArray, x._2)).toArray)
		   val numObsToTrain  = (trainValidRatio*labels.size).floor.toInt
		   val nb = NaiveBayes[Int](labels.take(numObsToTrain))
		   validate(labels.drop(numObsToTrain+1), nb)
	   } match {
   	      case Success(res) => Display.show(s"BinomialBayesEval.run: $description F1 = ${res.get}", logger)
      	  case Failure(e) => Display.error("BinomialBayesEval.run ", logger, e)
      }
	}
	
	private def validate(input:  XTSeries[(Array[Int], Int)], nb: NaiveBayes[Int]): Option[Double] = nb.validate(input, 0)
	
	
	private def load(ticker: String, period: Int): List[(List[Int], Int)] = {
	  val symbol = s"${ticker}.csv"
      val xs = DataSource(symbol, path, true) |> extractor
	  val mv = SimpleMovingAverage[Double](period)
     
	  val ratios: List[Array[Int]] = xs.map(x => {
	     val xt = mv get x.toArray
	     val zValues: Array[(Double, Double)] = x.drop(period).zip(xt.drop(period))
	     zValues.map(z => if(z._1 > z._2) 1 else 0).toArray
      })
	          
	           // Compute the difference in price. If the price(t+1) > price(t) then 
	           // the label is set to 1, 0 otherwise. We need to shift the index by +1
	           // by dropping the first period +1 price points.
	  var prev = xs(0)(period)
	  val label = xs(0).drop(period+1).map( x => {
	  val y = if( x > prev) 1 else 0
	     prev = x
	     y
	  }).toArray 
	             // Transpose the list of ratios and zip with the label
	  ratios.transpose.take(label.size).zip(label) 
	}
}

object BinomialNBApp extends App {
  val TRAIN_VALIDATION_RATIO = "0.8"
  BinomialBayesEval.run(Array[String]("NEM", TRAIN_VALIDATION_RATIO, "4"))
  BinomialBayesEval.run(Array[String]("NEM", TRAIN_VALIDATION_RATIO, "12"))
  BinomialBayesEval.run(Array[String]("NEM", TRAIN_VALIDATION_RATIO, "36"))
  
  BinomialBayesEval.run(Array[String]("GE", TRAIN_VALIDATION_RATIO, "4"))
  BinomialBayesEval.run(Array[String]("GE", TRAIN_VALIDATION_RATIO, "12"))
  BinomialBayesEval.run(Array[String]("GE", TRAIN_VALIDATION_RATIO, "36"))
  
  BinomialBayesEval.run(Array[String]("BAC", TRAIN_VALIDATION_RATIO, "4"))
  BinomialBayesEval.run(Array[String]("BAC", TRAIN_VALIDATION_RATIO, "12"))
  BinomialBayesEval.run(Array[String]("BAC", TRAIN_VALIDATION_RATIO, "36"))
}


// -----------------------------  EOF ------------------------------------