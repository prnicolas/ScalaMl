/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.trading



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
  	  try {
  	     (s(HIGH.id).toDouble/s(LOW.id).toDouble -1.0)*s(VOLUME.id).toDouble
   	  }
      catch {
         case e: RuntimeException => -1
         case e: NumberFormatException => -2
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



// ------------------------------  EOF ---------------------------------------------