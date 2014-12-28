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

import org.jfree.data.xy.{XYSeriesCollection, XYSeries}
import org.jfree.data.category.{DefaultCategoryDataset, CategoryDataset}
import org.jfree.data.statistics.DefaultMultiValueCategoryDataset
import org.jfree.data.xy.XYDataset
import org.jfree.chart.{ChartFactory, JFreeChart, ChartFrame}
import org.jfree.chart.plot.{PlotOrientation, XYPlot, CategoryPlot}
import org.jfree.chart.renderer.xy.{XYDotRenderer, XYLineAndShapeRenderer, XYShapeRenderer}
import org.jfree.chart.renderer.category.LineAndShapeRenderer
import org.jfree.chart.axis.{ValueAxis, NumberAxis}
import org.jfree.util.ShapeUtilities

import org.scalaml.core.Types.ScalaMl
import org.scalaml.util.DisplayUtils
import Plot._, ScalaMl._


		/**
		 * <p>Class to create a Line plot using the JFreeChart library.<br>
		 * @constructor Create a Line plot instance
		 * @throws IllegalArgumentException if the class parameters are undefined
		 * @param config  Configuration for the plot of type <b>PlotInfo</b>
		 * @param theme  Configuration for the display of plots of type <b>PlotTheme</b>
		 * @see org.jfree
		 * @author Patrick Nicolas
		 * @since  November 18, 2013
		 * @note Scala for Machine Learning
		 */
final class LinePlot(config: PlotInfo, theme: PlotTheme) extends Plot(config, theme)	{
	import java.awt.{GradientPaint, Color, Stroke, Shape, Paint, BasicStroke}
	import java.awt.geom.Ellipse2D

	private val colors: Array[Color] = Array[Color](Color.gray, Color.black, Color.red, Color.blue)
	private val shapes: Array[Shape] = Array[Shape](ShapeUtilities.createDiamond(1.0F), 
													ShapeUtilities.createRegularCross(1.0F, 1.0F),
													ShapeUtilities.createDownTriangle(2.0F))
													
		/**
		 * DisplayUtils array of tuple (x,y) in a Line plot for a given width and height
		 * @param xy Array of pair (x,y)
		 * @param width Width for the display (pixels)
		 * @param height Height of the chart (pixels)
		 * @throws IllegalArgumentException if the dataset is undefined or the width or height are 
		 * out of bounds.
		 */
	override def display(xy: XYTSeries, w: Int, h: Int): Unit  = {
		if( validateDisplayUtils[XY](xy, w, h, "LinePlot.display") ) {
			val catDataset = new DefaultCategoryDataset
			xy.foreach( x => catDataset.addValue(x._1, config._2, String.valueOf(x._2.toInt) ))
			draw(catDataset, w, h)
		}
	}		 

		/**
		 * DisplayUtils a vector of Double value in a Line plot with counts [0, n] on X-Axis and
		 * vector value on Y-Axis with a given width and height
		 * @param xy Array of pair (x,y)
		 * @param width Width for the display (pixels)
		 * @param height Height of the chart (pixels)
		 * @throws IllegalArgumentException if the dataset is undefined or the width or height are 
		 * out of bounds.
		 */
	override def display(y: DblVector, w: Int, h: Int): Unit  = {
		if( validateDisplayUtils[Double](y, w, h, "LinePlot.display") ) {
			val catDataset = new DefaultCategoryDataset
			y.zipWithIndex.foreach( x =>catDataset.addValue(x._1, config._2, String.valueOf(x._2)))
			draw(catDataset, w, h)
		}
	}

	import scala.collection.immutable.List
	def display(xys: List[(DblVector, String)], w: Int, h: Int): Unit  = {
		import scala.collection.JavaConversions._
		if( validateDisplayUtils[(DblVector, String)](xys, w, h, "LinePlot.display") ) {
	
			val seriesCollection = new XYSeriesCollection
			xys.foreach( xy => {   
				val xSeries = new XYSeries(xy._2)
				xy._1.zipWithIndex.foreach(z => xSeries.add(z._2.toDouble, z._1))
				seriesCollection.addSeries(xSeries)
			})
	
			val chart = ChartFactory.createXYLineChart(s"${config._1} - ${config._2}", config._2, config._3, 
										seriesCollection, 
										PlotOrientation.VERTICAL, true, true, false)
	
			val plot = chart.getXYPlot
			plot.setBackgroundPaint(theme.paint(w, h))
			plot.setDomainGridlinePaint(Color.lightGray)
			plot.setRangeGridlinePaint(Color.lightGray)
	
			val xyLineRenderer: XYLineAndShapeRenderer = plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]
	
			Range(0, xys.size) foreach( n => {
				xyLineRenderer.setSeriesPaint(n, colors(n % colors.size))
				xyLineRenderer.setSeriesShapesVisible(n, true)
				xyLineRenderer.setSeriesShape(n, shapes(n % shapes.size))
			})
			xyLineRenderer.setSeriesLinesVisible(0, false)
	  	  
			createFrame(s"${config._1}", chart)
		}
	}
	  
	private[this] def draw(catDataset: CategoryDataset, w: Int, h: Int): Unit = {
		val chart = ChartFactory.createLineChart(config._2, config._2, config._3, catDataset, 
								PlotOrientation.VERTICAL, false, false, false)
		val plot = chart.getPlot.asInstanceOf[CategoryPlot]
	    
		val renderer = new LineAndShapeRenderer {
			override def  getItemStroke(row: Int, col: Int): Stroke = theme.stroke(0)
		}
	 
		plot.setRenderer(renderer)
		renderer.setSeriesShape(0, ShapeUtilities.createDiamond(1.0F))
		renderer.setSeriesPaint(0, theme.color(0))

		plot.setBackgroundPaint(theme.paint(w, h))
		createFrame(config._1, chart)
	}
}


object LinePlot {
	private val DEFAULT_WIDTH = 320
	private val DEFAULT_HEIGHT = 260
	
	def display(
			y: DblVector, 
			labels: scala.collection.immutable.List[String], 
			theme: PlotTheme): Unit = 
		if( DisplayUtils.isChart )
			createPlot(y.isEmpty, labels, theme).display(y, DEFAULT_WIDTH, DEFAULT_WIDTH)
	
	def display(
			xys: List[(DblVector, String)], 
			labels: scala.collection.immutable.List[String], 
			theme: PlotTheme): Unit = 
		if( DisplayUtils.isChart )
			createPlot(xys.isEmpty, labels, theme).display(xys, DEFAULT_WIDTH, DEFAULT_WIDTH)
	
	private def createPlot(
			isEmpty: Boolean, 
			labels: scala.collection.immutable.List[String], 
			theme: PlotTheme): LinePlot = {
		
			import scala.collection.JavaConversions._
			require( !isEmpty, 
					s"$labels(0).display Cannot plot an undefined time series")
		
			val chartLabels = seqAsJavaList(labels)
			new LinePlot((chartLabels(1), chartLabels(2), chartLabels(3)), theme)
	}
}
// ------------------------  EOF ----------------------------------------------