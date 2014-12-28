/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.plots

import java.util.List
import java.awt.{GradientPaint, Color, Stroke, Shape, Paint, BasicStroke}
import java.awt.geom.Ellipse2D

import org.jfree.data.xy.XYDataset
import org.jfree.data.xy.{XYSeriesCollection, XYSeries}
import org.jfree.data.statistics.DefaultMultiValueCategoryDataset
import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.chart.plot.{PlotOrientation, XYPlot, CategoryPlot}
import org.jfree.chart.renderer.xy.XYShapeRenderer
import org.jfree.chart.axis.{ValueAxis, NumberAxis}
import org.jfree.chart.ChartFrame
import org.jfree.chart.renderer.xy.{XYDotRenderer, XYLineAndShapeRenderer}
import org.jfree.chart.renderer.category.LineAndShapeRenderer
import org.jfree.util.ShapeUtilities

import org.scalaml.core.Types.ScalaMl
import org.scalaml.util.DisplayUtils
import Plot._

object ChartType extends Enumeration {
	type ChartType = Value
	val LINE, TIME_SERIES, SCATTER, BAR = Value
}

		/**
		 * <p>Generic plotting class that uses the JFreeChart library.<br>
		 * @param config  Configuration for the plot of type <b>PlotInfo</b>
		 * @param theme Configuration for the display of plots of type <b>PlotTheme</b>
		 * @author Patrick Nicolas
		 * @since  November 18, 2013
		 * @note Scala for Machine Learning
		 */
abstract class Plot(config: PlotInfo, theme: PlotTheme) {
	import ScalaMl._


		/**
		 * DisplayUtils array of tuple (x,y) in a 2D plot for a given width and height
		 * @param xy Array of pair (x,y)
		 * @param width Width for the display (pixels)
		 * @param height Height of the chart (pixels)
		 */
	def display(xy: XYTSeries, width: Int, height: Int): Unit
	
	def display(xy: XYTSeries): Unit = display(xy, DEFAULT_WIDTH, DEFAULT_HEIGHT)

		/**
		 * DisplayUtils a vector of Double value in a 2D plot with counts [0, n] on X-Axis and
		 * vector value on Y-Axis with a given width and height
		 * @param xy Array of pair (x,y)
		 * @param width Width for the display (pixels)
		 * @param height Height of the chart (pixels)
		 */
	def display(y: DblVector, width: Int, height: Int): Unit
	
	def display(y: DblVector): Unit = display(y, DEFAULT_WIDTH, DEFAULT_HEIGHT)
   
	protected[this] def createFrame(id: String, chart: JFreeChart): Unit = {
		val frame = new ChartFrame(s"Chart ${count+1}: $id", chart)
		val anchor = getLocation
		frame.setLocation(anchor._1, anchor._2)
		frame.pack
		frame.setVisible(true)
	}
}


		/**
		 * Companion object for the Plot class
		 * @author Patrick Nicolas
		 * @since  November 18, 2013
		 * @note Scala for Machine Learning
		 */
object Plot {
	type PlotInfo = (String, String, String)
	final val DISPLAY_OFFSET = 25
	
	var count = 0
	final def getLocation: (Int, Int) = {
		count += 1
		val offset = count * DISPLAY_OFFSET
		(offset, offset % 420)
	}

	private val DEFAULT_WIDTH = 320
	private val DEFAULT_HEIGHT = 240
	
	private val MIN_DISPLAY_SIZE = 60
	private val MAX_DISPLAY_SIZE = 1280
	
		/**
		 * Validate the display dimension for a particular plot
		 * @param y Array of values to be plotted
		 * @param height  Height of the display
		 * @param width Width of the display
		 * @param comment Comments to be added to the chart or plot
		 * @throws IllegalArgumentException if the display height or width is out or range or 
		 * the values are undefined
		 */
	def validateDisplayUtils[T](y: Array[T], width: Int, height: Int, comment: String): Boolean = {
		if( DisplayUtils.isChart ) {
			require( !y.isEmpty, s"$comment Cannot display an undefined series")
			validateDisplaySize(width, height, comment)
	  }
		DisplayUtils.isChart
	}

		/**
		 * Validate the display dimension for a particular plot
		 * @param y List of values to be plotted
		 * @param height  Height of the display
		 * @param width Width of the display
		 * @param comment Comments to be added to the chart or plot
		 * @throws IllegalArgumentException if the display height or width is out or range or 
		 * the values are undefined
		 */
	import scala.collection.immutable.List
	def validateDisplayUtils[T](y: List[T], width: Int, height: Int, comment: String =""): Boolean = {
		if( DisplayUtils.isChart ) {
			require( !y.isEmpty, s"$comment Cannot display an undefined series")
			validateDisplaySize(width, height, comment)
		}
		DisplayUtils.isChart
	}
	
		/**
		 * Validate the display dimension for a particular plot
		 * @param height  Height of the display
		 * @param width Width of the display
		 * @param comment Comments to be added to the chart or plot
		 * @throws IllegalArgumentException if the display height or width is out or range
		 */
	def validateDisplaySize(width: Int, height: Int, comment: String = ""): Boolean = {
		if( DisplayUtils.isChart ) {
			require( width > MIN_DISPLAY_SIZE && width < MAX_DISPLAY_SIZE, 
					s"$comment Width $width is out of range")
			require( height > MIN_DISPLAY_SIZE && height < MAX_DISPLAY_SIZE, 
					s"$comment  height $height is out of range")
		}
		DisplayUtils.isChart
	}
}

// ------------------------  EOF ----------------------------------------------