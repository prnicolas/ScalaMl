/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.trading


		/**
		 * <p>Enumeration used extract data for evaluating risk on dividend pay-out. 
		 * Object methods are implemented to load the appropriate field and perform the type conversion</p>
		 * 
		 * @author Patrick Nicolas
		 * @since May 3, 2014
		 * @note Scala for Machine Learning
		 */
object DividendPayout extends Enumeration {
   type DividendPayout = Value
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

// -------------------------  EOF ------------------------------------------------------