/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap3



import org.scalaml.core.{Types, XTSeries}
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import YahooFinancials._


		/**
		 * Singleton used to test the moving average algorithms
		 */
object MovingAveragesEval extends FilteringEval {
   import org.scalaml.filtering._
   
   override def run(args: Array[String]): Unit = {
  	 Console.println("MovingAverages evaluation")
  	 
  	 val symbol = args(0)
     val p = args(1).toInt
     val p_2 = p >>1
     val w = Array.tabulate(p)(n => if( n == p_2) 1.0 else 1.0/(Math.abs(n -p_2)+1))
     val weights = w map { _ / w.sum }

     weights.foreach( x => println( x + ",") )
     val dataSource = DataSource("resources/data/chap3/" + symbol + ".csv", false)
     
	 dataSource |> YahooFinancials.adjClose match {
	    case Some(price) => {  
	      val sMvAve = SimpleMovingAverage[Double](p)  
	      val wMvAve = WeightedMovingAverage[Double](weights)
	      val eMvAve = ExpMovingAverage[Double](p)
	
	      val dataSink = DataSink[Double]("output/chap3/mvaverage" + p.toString + ".csv")

	      val results = price :: sMvAve.|>(price).getOrElse(price) :: 
	                             sMvAve.|>(price).getOrElse(price) :: 
	                             sMvAve.|>(price).getOrElse(price) :: 
	                             List[XTSeries[Double]]()
	      dataSink |> results
	    }
	    case None => Console.println("cannot load data")  
	  }
    }
}



// --------------------------------------  EOF -------------------------------