package org.scalaml.app.chap5


import org.scalaml.supervised.bayes._
import org.scalaml.core.{Types, XTSeries}
import org.scalaml.workflow.data.{DataSource,TextSource}
import scala.collection.mutable.ArrayBuffer
import Types.ScalaMl._
import org.scalaml.filtering.SimpleMovingAverage
import SimpleMovingAverage._
import scala.collection.immutable.HashSet
import org.scalaml.supervised.bayes.MultinomialNaiveBayes

trait BayesEval {
   import org.scalaml.trading.PriceVolume._
   final val path = "resources/data/chap5/"
       
   def run(args: Array[String]): Unit
  
   protected val extractor = toDouble(CLOSE) :: 
                             % (HIGH, LOW) :: 
                             toDouble(VOLUME) ::
                             List[Array[String] =>Double]()
   protected def symbols = DataSource.listSymbols(path)
}



object Dim2BayesEval extends BayesEval {
	
	override def run(args: Array[String]): Unit = {
	   println("Evaluation Multinomial Naive Bayes")	
		
	   val trainValidRatio = args(1).toDouble
	   val period = args(2).toInt
	   
	   load(args, period) match {
		 case Some(input) => {
			val labels = XTSeries[(Array[Int], Int)](input.map( x => (x._1.toArray, x._2)).toArray)
			val numObsToTrain  = (trainValidRatio*labels.size).floor.toInt
			val nb = MultinomialNaiveBayes[Int](labels.take(numObsToTrain))
		    validate(labels.drop(numObsToTrain+1), nb)
		 }
		 case None => Console.println("Error")
	   }
	}
	
	private def validate(input:  XTSeries[(Array[Int], Int)], nb: MultinomialNaiveBayes[Int]): Double = nb.validate(input, 0)
	
	
	private def load(args: Array[String], period: Int): Option[List[(List[Int], Int)]] = {
	  val symbol = args(0)
	  
      try {
		 DataSource(symbol, path, true) |> extractor match {
	  
	       case Some(xs) => {
	          val em = SimpleMovingAverage[Double](period)
     
	          val ratios: List[Array[Int]] = xs.map( values => 
	             em |> values match { 
	                case Some(xt) => 
	                	values.drop(period).zip(xt.drop(period)).map( z => if( z._1 > z._2) 1 else 0)
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
	           }).arr 
	             // Transpose the list of ratios and zip with the label
	           Some(ratios.transpose.take(label.size).zip(label))
	       }
	       case None => throw new IllegalStateException("Moving Average failed") 
	    }  
      }
      catch {
      	 case e: IllegalStateException => None
      }
	}
}


// -----------------------------  EOF ------------------------------------