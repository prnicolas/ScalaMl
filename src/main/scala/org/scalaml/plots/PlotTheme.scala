/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.plots

import java.awt.{GradientPaint, Color, Stroke, Shape, Paint, BasicStroke}


		/**
		 * <p>Generic trait for visual display of a plotting graph using jFreeChart library</p>
		 * @see org.jfree
		 * @author Patrick Nicolas
		 * @since  November 20, 2013
		 * @note Scala for Machine Learning Chapter 2 Hello World!
		 */
trait PlotTheme {
	import BasicStroke._
	
	protected[this] val strokeList = Array[Stroke](
		new BasicStroke(1.0f),
		new BasicStroke(1.0f, CAP_BUTT, JOIN_BEVEL, 8.0f, Array[Float](1.0f, 1.0f), 0.0f),
		new BasicStroke(1.0f, CAP_BUTT, JOIN_BEVEL, 8.0f,
				Array[Float](3.0f, 3.0f), 0.0f)
	)

	def stroke(index: Int) = strokeList(index % strokeList.size)
	
		/**
		 * <p>Select the color from an existing palette or list compatible
		 * with the background of the plot.</p>
		 * @param index Index of the color from the palette
		 * @return color for a specific component of the plot
		 */
	def color(index: Int): Color
	
		/**
		 * Define the background color of the plot
		 * @param width Width of the chart
		 * @param height Height of the chart
		 * @return Background color 
		 */
	def paint(width: Int, height: Int): Paint
}


		/**
		 * <p>Class that define the visual display of a plotting graph using jFreeChart library
		 * with a black background. The color of the data points, graphs, labels.. are set accordingly.</p>
		 * @see org.jfree
		 * @author Patrick Nicolas
		 * @since  November 20, 2013
		 * @note Scala for Machine Learning Chapter 2 Hello World!
		 */
final class BlackPlotTheme extends PlotTheme {
	private[this] val colorList = Array[Color](Color.white, Color.cyan, Color.yellow)

		/**
		 * <p>Select the color from an existing palette or list compatible
		 * with the background of the plot.</p>
		 * @param index Index of the color from the palette
		 * @return color for a specific component of the plot
		 */
	override def color(index: Int): Color = colorList(index % colorList.size)
	
		/**
		 * Define the background color of the plot at black
		 * @param width Width of the chart
		 * @param height Height of the chart
		 * @throws IllegalArgumentException if the width or height is out of range
		 * @return Background color 
		 */
	override def paint(width: Int, height: Int): Paint = {
		Plot.validateDisplaySize(width, height)
		Color.black
	}
}

		/**
		 * <p>Class that define the visual display of a plotting graph using jFreeChart library
		 * with a light grey background with gradient. The color of the data points, 
		 * graphs, labels.. are set accordingly.</p>
		 * @see org.jfree
		 * @author Patrick Nicolas
		 * @since  November 20, 2013
		 * @note Scala for Machine Learning Chapter 2 Hello World!
		 */
final class LightPlotTheme extends PlotTheme {
	private[this] val colorList = Array[Color](Color.black, Color.red, Color.blue)

		/**
		 * <p>Select the color from an existing palette or list compatible
		 * with the background of the plot.</p>
		 * @param index Index of the color from the palette
		 * @return color for a specific component of the plot
		 */
	override def color(index: Int): Color = colorList(index % colorList.size)
	
		/**
		 * Define the background color of the plot as a gradient of light gray
		 * @param width Width of the chart
		 * @param height Height of the chart
		 * @throws IllegalArgumentException if the width or height is out of range
		 * @return Background color 
		 */
	override def paint(width: Int, height: Int): Paint = {
			Plot.validateDisplaySize(width, height)
			new GradientPaint(0, 0, Color.white, width, height, Color.lightGray, false)
	}
}

// ----------------------------------  EOF --------------------------------------------------------------