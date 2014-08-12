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
		 * <p>Singleton used to evaluate the Discrete Sine and Cosine Fourier transform.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since February 8, 2014
		 * @note Scala for Machine Learning
		 */
object DFTEval extends FilteringEval {
   import Types.ScalaMl._
   import org.scalaml.filtering.DFT
   
   def run(args: Array[String]): Unit = {
     require(args != null && args.length > 0, "Incorrect argument: required DFTEval.run symbolName")
     
     Console.println("Evaluation of Discrete Fourier series")
     val symbol = args(0)
     val src = new DataSource("resources/data/chap3/" + symbol + ".csv", false, true)
     
     src |> YahooFinancials.adjClose match {
       
        case Some(price) => {  
          val filter = DFT[Double]
          val sinkPrice =  DataSink[Double]("output/chap3/" + symbol + "_.csv")
          sinkPrice |> price :: List[XTSeries[Double]]()
         
          filter |> price match {
             case Some(xtSeries) => {
          	    val res: DblVector = xtSeries
                val thresholdValue = res.max*0.01
                val sink2 = DataSink[Double]("output/chap3/filt_" + symbol + ".csv")
                sink2 |>  XTSeries[Double](res.map( x => if(x > thresholdValue) x else 0.0)) :: List[XTSeries[Double]]()
             }
             case None => Console.println("Could not filter " + symbol);
          }
       }
        case None=> Console.println("Could not filter " + symbol);
     }
   }
}

// --------------------------------------  EOF -------------------------------