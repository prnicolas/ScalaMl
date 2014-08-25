/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap5


import org.scalaml.supervised.bayes._
import org.scalaml.core.{Types, XTSeries}
import org.scalaml.workflow.data.{DataSource,DocumentsSource}
import scala.collection.mutable.ArrayBuffer
import Types.ScalaMl._
import org.scalaml.filtering.SimpleMovingAverage
import SimpleMovingAverage._
import scala.collection.immutable.HashSet
import org.scalaml.supervised.bayes.NaiveBayes
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display



trait BayesEval {
   import org.scalaml.trading.YahooFinancials._
   final val path = "resources/data/chap5/"
       
   def run(args: Array[String]): Unit
  
   protected val extractor = toDouble(CLOSE) :: 
                             % (HIGH, LOW) :: 
                             toDouble(VOLUME) ::
                             List[Array[String] =>Double]()
   protected def symbols = DataSource.listSymbols(path)
}



object BinomialBayesEval extends BayesEval {
	private val logger = Logger.getLogger("BinomialBayesEval")
	
	override def run(args: Array[String]): Unit = {
	   println("Evaluation Multinomial Naive Bayes")	
		
	   val trainValidRatio = args(1).toDouble
	   val period = args(2).toInt
	   
	   load(args, period) match {
		 case Some(input) => {
			val labels = XTSeries[(Array[Int], Int)](input.map( x => (x._1.toArray, x._2)).toArray)
			val numObsToTrain  = (trainValidRatio*labels.size).floor.toInt
			val nb = NaiveBayes[Int](labels.take(numObsToTrain))
		    validate(labels.drop(numObsToTrain+1), nb)
		 }
		 case None => Console.println("Error")
	   }
	}
	
	private def validate(input:  XTSeries[(Array[Int], Int)], nb: NaiveBayes[Int]): Double = nb.validate(input, 0)
	
	
	private def load(args: Array[String], period: Int): Option[List[(List[Int], Int)]] = {
	  val symbol = args(0)
	  
      Try {
		 DataSource(symbol, path, true) |> extractor match {
	  
	       case Some(xs) => {
	          val mv = SimpleMovingAverage[Double](period)
     
	          val ratios: List[Array[Int]] = xs.map(x => 
	             mv |> x match { 
	                case Some(xt) => 
	                	x.drop(period).zip(xt.drop(period)).map( z => if( z._1 > z._2) 1 else 0).toArray
	            	case None => throw new IllegalStateException("Moving Average failed") 
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
	       case None => throw new IllegalStateException("Moving Average failed") 
	    }  
      } match  {
      	case Success(res) => Some(res)
      	case Failure(e) => Display.error("BinomialBayesEval ", logger, e); None
      }
	}
}


// -----------------------------  EOF ------------------------------------