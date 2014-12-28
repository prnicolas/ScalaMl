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
	
import org.jfree.data.xy.{XYSeriesCollection, XYSeries, XYDataset}
import org.jfree.data.statistics.DefaultMultiValueCategoryDataset
import org.jfree.data.category.{DefaultCategoryDataset, CategoryDataset}
import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.chart.plot.{PlotOrientation, XYPlot, CategoryPlot}
import org.jfree.chart.ChartFrame
import org.jfree.chart.renderer.xy.{XYDotRenderer, XYLineAndShapeRenderer, XYShapeRenderer}
import org.jfree.chart.renderer.category.LineAndShapeRenderer
import org.jfree.chart.axis.{ValueAxis, NumberAxis}
import org.jfree.util.ShapeUtilities

import Plot._

	/**
		 * <p>Class to create a Scatter plot using the JFreeChart library.</p>
		 * @constructor Create a Scatter plot instance
		 * @param config  Configuration for the plot of type <b>PlotInfo</b>
		 * @param theme   Configuration for the display of plots of type <b>PlotTheme</b>
		 * @throws IllegalArgumentException if the class parameters are undefined
		 * @see org.jfree
		 * @author Patrick Nicolas
		 * @since  November 18, 2013
		 * @note Scala for Machine Learning
		 */
final class ScatterPlot(config: PlotInfo, theme: PlotTheme) extends Plot(config, theme) {
	import java.awt.{GradientPaint, Color, Stroke, Shape, Paint, BasicStroke}
	import java.awt.geom.Ellipse2D
	import org.scalaml.core.Types.ScalaMl._
	
		/**
		 * DisplayUtils array of tuple (x,y) in a Scatter plot for a given width and height
		 * @param xy Array of pair (x,y)
		 * @param width Width for the display (pixels)
		 * @param height Height of the chart (pixels)
		 * @throws IllegalArgumentException if the dataset is undefined or the width or height 
		 * are out of bounds.
		 */
	override def display(xy: XYTSeries, width: Int, height : Int): Unit  = {
		if( validateDisplayUtils(xy, width, height, "ScatterPlot.display") ) {
			val series =  xy.foldLeft(new XYSeries(config._1))((s, xy) => {s.add(xy._1, xy._2); s})
			val seriesCollection = new XYSeriesCollection
			seriesCollection.addSeries(series)
			draw(seriesCollection, width, height)
		}
	}
	
		/**
		 * DisplayUtils a vector of Double value in a Scatter plot with counts [0, n] on X-Axis and
		 * vector value on Y-Axis with a given width and height
		 * @param xy Array of pair (x,y)
		 * @param width Width for the display (pixels)
		 * @param height Height of the chart (pixels)
		 * @throws IllegalArgumentException if the dataset is undefined or the width or height 
		 * are out of bounds.
		 */
	override def display(y: DblVector, width: Int, height: Int): Unit = {
		if( validateDisplayUtils[Double](y, width, height, "ScatterPlot.display") ) {
			val series =  y.zipWithIndex.foldLeft(new XYSeries(config._1))((s, xn) => {
					s.add(xn._1, xn._2)
					s
			})
			val seriesCollection = new XYSeriesCollection
			seriesCollection.addSeries(series)
			draw(seriesCollection, width, height)
		}
	}
   

		/**
		 * DisplayUtilss two two-dimension datasets (x, y) in this scatter plot
		 * @param xy1 First Array of pair (x,y) to be displayed
		 * @param xy2 Second Array of pair (x,y) to be displayed
		 * @param width Width for the display (pixels)
		 * @param height Height of the chart (pixels)
		 * @throws IllegalArgumentException if either dataset is undefined or the width or height 
		 * are out of bounds.
		 */
	def display(xy1: XYTSeries, xy2: XYTSeries, width: Int, height : Int): Unit  = {
		require( !xy1.isEmpty, 
				"ScatterPlot.DisplayUtils Cannot display with first series undefined")
		require( !xy1.isEmpty, 
				"ScatterPlot.DisplayUtils Cannot display with second series undefined ")

		if( validateDisplaySize(width, height, "ScatterPlot.DisplayUtils") ) {

			val seriesCollection1 = new XYSeriesCollection
			val seriesCollection2 = new XYSeriesCollection
			val series1 = xy1.foldLeft(new XYSeries(config._1))((s, xy) => {s.add(xy._1, xy._2); s})
			val series2 = xy2.foldLeft(new XYSeries(config._1))((s, xy) => {s.add(xy._1, xy._2); s})
			seriesCollection1.addSeries(series1)
			seriesCollection2.addSeries(series2)
		  
			val chart = ChartFactory.createScatterPlot(config._2, config._2, config._3, seriesCollection1, 
						PlotOrientation.VERTICAL, true, false, false)
	      
			val plot = chart.getPlot.asInstanceOf[XYPlot]
			val renderer1 = new XYDotRenderer
			plot.setRenderer(0, renderer1)
			plot.setDomainAxis(0, new NumberAxis("x"))
			renderer1.setSeriesPaint(0, theme.color(0))
			renderer1.setDotHeight(4)
			renderer1.setDotWidth(4)
	      
			plot.setDataset(1, seriesCollection2)
			val renderer2 = new XYShapeRenderer
			plot.setRenderer(1, renderer2)
			renderer2.setSeriesShape(0, ShapeUtilities.createDiamond(3.0F))
			renderer2.setSeriesPaint(0, theme.color(1))
	   
			plot.setBackgroundPaint(theme.paint(width, height))
			createFrame(config._1, chart)	
		}
	}

	import scala.collection.immutable.List
	def display(xs: List[XYTSeries], lbls: List[String], w: Int, h : Int): Unit  = {
		if( validateDisplayUtils[XYTSeries](xs, w, h, "ScatterPlot.display") ) {
	
			val seriesCollectionsList = xs.zipWithIndex
					.foldLeft(scala.List[XYSeriesCollection]())((xs, xy) => { 
				val seriesCollection = new XYSeriesCollection
				val series = xy._1.foldLeft(new XYSeries(lbls(xy._2)))((s, t) => {
					s.add(t._1, t._2)
					s
				})
				seriesCollection.addSeries(series)
				seriesCollection :: xs
			})
		  
			val chart = ChartFactory.createScatterPlot(config._2, config._2, config._3, seriesCollectionsList.last, 
						PlotOrientation.VERTICAL, true, false, false)
			val plot = chart.getPlot.asInstanceOf[XYPlot]
	  	  
			val renderer1 = new XYDotRenderer
			plot.setRenderer(0, renderer1)
			plot.setDomainAxis(0, new NumberAxis("Trading sessions"))
			renderer1.setSeriesPaint(0, theme.color(0))
			renderer1.setDotHeight(4)
			renderer1.setDotWidth(4)
	   
			val shapes = Array[Shape](ShapeUtilities.createDiamond(3.0F), 
					ShapeUtilities.createRegularCross(2.0F, 2.0F))
			
			seriesCollectionsList.dropRight(1).zipWithIndex.foreach( sColi => { 
				plot.setDataset(sColi._2+1, sColi._1)
				val renderer = new XYShapeRenderer
				plot.setRenderer(sColi._2+1, renderer)
				renderer.setSeriesShape(0, shapes(sColi._2%shapes.size))
				renderer.setSeriesPaint(0, theme.color(sColi._2+1))
			})
	   
			plot.setBackgroundPaint(theme.paint(w, h))
			createFrame(config._1, chart)
		}
	}
   
	
	private def draw(seriesCollection: XYSeriesCollection, w: Int, h : Int) {	
		validateDisplaySize(w, h, "ScatterPlot.draw")
		
		val chart = ChartFactory.createScatterPlot(config._2, config._2, config._3, seriesCollection, 
			PlotOrientation.VERTICAL, true, false, false)
		val plot = chart.getPlot.asInstanceOf[XYPlot]
		
		val renderer = new XYDotRenderer
		plot.setRenderer(renderer)
		renderer.setSeriesShape(0, ShapeUtilities.createDiamond(4.0F))
		renderer.setSeriesPaint(0, theme.color(3))
		renderer.setDotHeight(3)
		renderer.setDotWidth(3)
   
		plot.setBackgroundPaint(theme.paint(w, h))
		createFrame(config._1, chart)
	}
}


object ScatterPlot {
	import org.scalaml.core.Types.ScalaMl.{DblVector, XYTSeries}
	
	private val DEFAULT_WIDTH = 320
	private val DEFAULT_HEIGHT = 260
	
	def display(
			y: DblVector,
			labels: scala.collection.immutable.List[String],
			theme: PlotTheme): Unit = {
		import scala.collection.JavaConversions._
		require( !y.isEmpty, 
				s"$labels(0).display Cannot plot an undefined time series")
		
		val chartLabels = seqAsJavaList(labels)
		val plotter = new ScatterPlot((chartLabels(1), chartLabels(2), chartLabels(3)), theme)
		plotter.display(y, DEFAULT_WIDTH, DEFAULT_WIDTH)
	}
	
	
	def display(
			xySeries: XYTSeries, 
			labels: scala.collection.immutable.List[String], 
			theme: PlotTheme): Unit = display(xySeries, Array.empty, labels, theme)
	
	def display(
			xySeries1: XYTSeries, 
			xySeries2: XYTSeries, 
			labels: scala.collection.immutable.List[String], 
			theme: PlotTheme): Unit = {
	  
		import scala.collection.JavaConversions._
		require( !xySeries1.isEmpty, 
				s"$labels(0).display Cannot plot an undefined time series")
		
		val chartLabels = seqAsJavaList(labels)
		val plotter = new ScatterPlot((chartLabels(1), chartLabels(2), chartLabels(3)), theme)
		
		if( !xySeries2.isEmpty)
			plotter.display(xySeries1, xySeries2, DEFAULT_WIDTH, DEFAULT_WIDTH)
		else
			plotter.display(xySeries1, DEFAULT_WIDTH, DEFAULT_WIDTH)
	}
}

// ------------------------  EOF ----------------------------------------------