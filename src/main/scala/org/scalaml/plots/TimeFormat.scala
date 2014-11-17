/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.plots

import org.jfree.chart._
import org.jfree.chart.plot.XYPlot
import org.jfree.ui.{ApplicationFrame, RectangleInsets, RefineryUtilities}
import org.jfree.data.xy.XYDataset
import org.jfree.chart.axis.DateAxis
import org.jfree.chart.renderer.xy.{XYItemRenderer, XYLineAndShapeRenderer}

import java.awt.{Color, Dimension}
import java.text.SimpleDateFormat
import java.util.Date




import  org.jfree.data.time._

  
  import TimeFormatModule._
  
  
class TimeFormat(val timeScale: Int, dateInput: Array[Date]) {
  require(timeScale <7, "Time scale out of bounds")
     
  def this(timeScale: Int, timeInput: Array[Long]) = this(timeScale, timeInput map(new Date( _)))
     
  def getTimePeriodObj(index: Int): RegularTimePeriod = {
      timeScale match {
         case 0 => new Millisecond(dateInput(index))
         case 1 => new Second(dateInput(index))
         case 2 => new Minute(dateInput(index))
         case 3 => new Hour(dateInput(index))
         case 4 => new Day(dateInput(index))
         case 5 => new Month(dateInput(index))
         case 6 => new Month(dateInput(index))
       }
   }
     
   def getTimePeriodDesc: SimpleDateFormat = new SimpleDateFormat(TIME_SCALE_FORMAT(timeScale))
}



object TimeFormatModule {
   final val MILLISECONDS 	= 0
   final val SECONDS 		= 1
   final val MINUTES 		= 2
   final val HOURS 			= 3
   final val DAYS 			= 4
   final val MONTHS			= 5
   final val YEARS 			= 6
   final val TIME_SCALE_FORMAT = Array[String]("SSSS", "ss:SSS", "mm:ss", "hh:mm:ss", "DDD-hh:mm:ss", "dd-MMM-yyyy", "MMM-yyyy")
}
// -----------------------  EOF -------------------------------------------