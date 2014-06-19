/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.trading

	/**
	 * <p>Singleton that descrive the differen
	 *  @author Patrick Nicolas
	 *  @date Feb 17, 2014
	 *  @project Book
	 */
object PriceVolume extends Enumeration {
  type PriceVolume = Value
  val DATE, OPEN, HIGH, LOW, CLOSE, VOLUME, ADJ_CLOSE = Value
  
  val adjClose = ((s:Array[String]) => s(ADJ_CLOSE.id).toDouble)
  val volume =  ((s:Array[String]) => s(VOLUME.id).toDouble)
  val volatility = ((s:Array[String]) => s(HIGH.id).toDouble - s(LOW.id).toDouble)
  val volatilityVol = ((s:Array[String]) => ((s(HIGH.id).toDouble - s(LOW.id).toDouble), s(VOLUME.id).toDouble))
  
  def toDouble(value: Value): Array[String] => Double = (s: Array[String]) => s(value.id).toDouble
  def /(value1: Value, value2: Value): Array[String] => Double = (s: Array[String]) => s(value1.id).toDouble/s(value2.id).toDouble
  def %(value1: Value, value2: Value): Array[String] => Double = (s: Array[String]) => try { s(value1.id).toDouble/s(value2.id).toDouble - 1.0 } catch { case e: NumberFormatException => s.foreach(println); 1.0} 
  def +(value1: Value, value2: Value): Array[String] => Double = (s: Array[String]) => s(value1.id).toDouble + s(value2.id).toDouble
  def *(value1: Value, value2: Value): Array[String] => Double = (s: Array[String]) => s(value1.id).toDouble * s(value2.id).toDouble
}



// ------------------------------  EOF ---------------------------------------------