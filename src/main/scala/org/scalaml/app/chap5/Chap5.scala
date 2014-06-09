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
       
   def eval(args: Array[String]): Unit
  
   protected val extractor = toDouble(CLOSE) :: 
                             % (HIGH, LOW) :: 
                             toDouble(VOLUME) ::
                             List[Array[String] =>Double]()
   protected def symbols = DataSource.listSymbols(path)
}



object Dim2BayesEval extends BayesEval {
	
	override def eval(args: Array[String]): Unit = {
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




object TextBayesEval {
	final val pathName = "resources/text/chap5/"
		
	private def tStamp(date: String): Int = {
		val idx1 = date.indexOf(".")
		val idx2 = date.lastIndexOf(".")
		if( idx1 != -1 && idx2 != -1) (date.substring(0, idx1) + date.substring(idx1+1, idx2)).toInt else -1
	}

    val lexicon = Map[String, String](
	   "tesla" -> "Tesla", "tsla" -> "TSLA" , "musk" -> "Musk", "china" -> "China", "chinese" -> "China", "charging" -> "Charge", 
	   "supercharger" -> "Charge", "battery" -> "battery", "batteries" ->"battery", "upside" ->"Upside", "outperform" ->"Upside", 
	   "risk" -> "risk", "risks" ->"risk", "risky"->"risk", "panasonic" ->"Panasonic", "growth" -> "Upside", "short" -> "Downside",
	   "shorted" -> "Downside", "downside" -> "Downside", "underperform" -> "Downside"
	)
	
	def parser(content: String): Array[String] = {
    	val regExpr = "['|,|.|?|!|:|\"]"
    	content.trim.toLowerCase.replace(regExpr," ").split(" ").filter( s => s.length > 2)
    }
    
    val quotes = Array[Double](
    	250.56, 254.84, 252.66, 252.94, 246.21, 238.84, 234.41, 241.49, 237.79, 230.97, 233.98, 240.04, 235.84,
        234.91, 228.89, 220.17, 220.44, 212.96, 207.32, 212.37, 208.45, 216.97, 230.29, 225.4, 212.22, 207.52,
        215.46, 216.93, 204.19, 203.78, 198.09, 193.91, 199.11, 198.12, 204.38, 218.64, 207.99, 207.86, 199.85 )
		
	def eval: Option[MultinomialNaiveBayes[Double]]  = {
		val textSource = TextSource(pathName)
		textSource.load match {
		   case Some(corpus) => {
			  NaiveBayesTextScoring(tStamp, parser, lexicon).score(corpus) match {
		  	  	case Some(keywords) => {
		  	  	  var prevQuote = 0.0
		  	  	  val diff = quotes.map( q => {val delta = if(q > prevQuote) 1 else 0; prevQuote = q; delta} )
		  	  	  diff foreach( q => println(q + ",") )
		  	  	  val freqLabels = keywords.toOrderedArray
		  	  	                       .zip(diff)
		  	  	                       .map( x => (x._1._2, x._2))
		  	  	                       .map( lbl => { (lexicon.values.toArray.map( f => if( lbl._1.contains(f) ) lbl._1.get(f).get else 0.0), lbl._2) })
			  	  Some(MultinomialNaiveBayes[Double](XTSeries[(Array[Double], Int)](freqLabels)))
		  	  	}
		  	  	case None => println("Naive Bayes failed"); None
		  	  }
		   }
		   case None => Console.println("Failed to load text"); None
		}
	}
}



object Chap5 extends App {

	TextBayesEval.eval
}

// -----------------------------  EOF ------------------------------------