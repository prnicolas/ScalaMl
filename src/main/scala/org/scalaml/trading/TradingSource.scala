/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.trading

import scala.util.{Try, Success, Failure}
import org.scalaml.util.Display


	/**
	 *  Singleton that describes the fields used in the extraction of price related data from the Yahoo
	 *  finances historical data. The data is loaded from a CSV file.
	 *  
	 *  @author Patrick Nicolas
	 *  @since Feb 17, 2014
	 *  @note Scala for Machine Learning
	 */
object YahooFinancials extends Enumeration {
   type YahooFinancials = Value
   val DATE, OPEN, HIGH, LOW, CLOSE, VOLUME, ADJ_CLOSE = Value
  
   val adjClose = ((s:Array[String]) => s(ADJ_CLOSE.id).toDouble)
   val volume =  ((s:Array[String]) => s(VOLUME.id).toDouble)
   val volatility = ((s:Array[String]) => s(HIGH.id).toDouble - s(LOW.id).toDouble)
   val volatilityVol = ((s:Array[String]) => ((s(HIGH.id).toDouble - s(LOW.id).toDouble), s(VOLUME.id).toDouble))
  
   def toDouble(value: Value): Array[String] => Double = (s: Array[String]) => s(value.id).toDouble
   def /(value1: Value, value2: Value): Array[String] => Double = (s: Array[String]) => s(value1.id).toDouble/s(value2.id).toDouble
   def %(value1: Value, value2: Value): Array[String] => Double = (s: Array[String]) => try { s(value1.id).toDouble/s(value2.id).toDouble - 1.0 } catch { case e: NumberFormatException => -1.0} 
   def +(value1: Value, value2: Value): Array[String] => Double = (s: Array[String]) => s(value1.id).toDouble + s(value2.id).toDouble
   def *(value1: Value, value2: Value): Array[String] => Double = (s: Array[String]) => s(value1.id).toDouble * s(value2.id).toDouble
   def vol: Array[String] => Double = (s: Array[String]) => {
  	  Try ( (s(HIGH.id).toDouble/s(LOW.id).toDouble -1.0)*s(VOLUME.id).toDouble  )
  	   match {
  	  	case Success(res) => res
  	  	case Failure(e) => Display.error("YahooFinancials.vol ", e); -1
  	  }
   }
}


object GoogleFinancials extends Enumeration {
   type GoogleFinancials = Value
   val DATE, OPEN, HIGH, LOW, CLOSE, VOLUME = Value
  
   val close = ((s:Array[String]) => s(CLOSE.id).toDouble)
   val volume =  ((s:Array[String]) => s(VOLUME.id).toDouble)
   val volatility = ((s:Array[String]) => s(HIGH.id).toDouble - s(LOW.id).toDouble)
   val volatilityVol = ((s:Array[String]) => ((s(HIGH.id).toDouble - s(LOW.id).toDouble), s(VOLUME.id).toDouble))
}



		/**
		 * <p>Enumeration used extract data for evaluating risk on dividend pay-out. 
		 * Object methods are implemented to load the appropriate field and perform the type conversion</p>
		 * 
		 * @author Patrick Nicolas
		 * @since May 3, 2014
		 * @note Scala for Machine Learning
		 */
object Fundamentals extends Enumeration {
   type Fundamentals = Value
   val TICKER, 
       START_PRICE, 
       END_PRICE, 
       RELATIVE_PRICE_CHANGE, 
       DEBT_TO_EQUITY, 
       DIVIDEND_COVERAGE, 
       OPERATING_MARGIN, 
       SHORT_INTEREST, 
       CASH_PER_SHARE,
       CASH_PER_SHARE_TO_PRICE, 
       EPS_TEND, 
       DIVIDEND_YIELD,
       DIVIDEND_TREND = Value
       
   val ticker = ((s:Array[String]) => s(TICKER.id))
   val relPriceChange =  ((s:Array[String]) => s(RELATIVE_PRICE_CHANGE.id).toDouble)
   val debtToEquity = ((s:Array[String]) => s(DEBT_TO_EQUITY.id).toDouble)
   val dividendCoverage = ((s:Array[String]) => s(DIVIDEND_COVERAGE.id).toDouble)
   val shortInterest = ((s:Array[String]) => s(SHORT_INTEREST.id).toDouble)
   val cashPerShareToPrice = ((s:Array[String]) => s(CASH_PER_SHARE_TO_PRICE.id).toDouble)
   val epsTrend = ((s:Array[String]) => s(EPS_TEND.id).toDouble)
   val dividendYield = ((s:Array[String]) => s(DIVIDEND_YIELD.id).toDouble)
   val dividendTrend = ((s:Array[String]) => s(DIVIDEND_TREND.id).toDouble)
}




// ------------------------------  EOF ---------------------------------------------