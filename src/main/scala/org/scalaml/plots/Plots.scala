/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.plots



import java.awt.{GradientPaint, Color, Stroke, Shape, Paint, BasicStroke}
import org.jfree.data.xy.{XYSeriesCollection, XYSeries}
import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.chart.plot.{PlotOrientation, XYPlot, CategoryPlot}
import org.jfree.chart.ChartFrame
import org.jfree.util.ShapeUtilities
import org.jfree.chart.renderer.xy.{XYDotRenderer, XYLineAndShapeRenderer}
import Plot._


import org.jfree.data.category.{DefaultCategoryDataset, CategoryDataset}
import org.jfree.chart.renderer.category.LineAndShapeRenderer
import org.jfree.data.statistics.DefaultMultiValueCategoryDataset
import java.util.List
import org.scalaml.core.Types



object ChartType extends Enumeration {
	type ChartType = Value
	val LINE, TIME_SERIES, SCATTER, BAR = Value
}


		/**
		 * <p>Generic base class for plots using the JFreeChart library.
		 */
import Types.ScalaMl._
abstract class Plot(val config: PlotInfo, val theme: PlotTheme) {
   require(config != null, "Cannot create a plot with undefined configuration")
   require(theme != null, "Cannot create a plot with undefined theme")
   
   def display(xy: XYTSeries, w: Int, h: Int): Unit
  
   def display(y: DblVector, w: Int, h: Int, step: Int): Unit = { }
   
   protected[this] def createFrame(id: String, chart: JFreeChart): Unit = {
  	 val frame = new ChartFrame("First", chart)
	 frame.pack
	 frame.setVisible(true)
   }
}


class LinePlot(val _config: PlotInfo, val _theme: PlotTheme) extends Plot(_config, _theme)	{
   private val colors: Array[Color] = Array[Color](Color.gray, Color.black)
   private val shapes: Array[Shape] = Array[Shape](ShapeUtilities.createDiamond(1.0F), 
  		                                          ShapeUtilities.createRegularCross(1.0F, 1.0F),
  		                                          ShapeUtilities.createDownTriangle(2.0F))
  		                                          
   override def display(xy: XYTSeries, w: Int, h: Int): Unit  = {
  	  require( xy != null && xy.size > 0, "Cannot display an undefined series")
      require( w > 60 && w < 1260, "Width " + w + " is out of range")
      require( h > 60 && h < 1260, "height " + h + " is out of range")
       
  	  val catDataset = new DefaultCategoryDataset
  	  xy.foreach( x => catDataset.addValue(x._1, config._2, String.valueOf(x._2.toInt) ))
      draw(catDataset, w, h)
   }		 
   
   override def display(y: DblVector, w: Int, h: Int, step: Int): Unit  = {
  	  require(y != null && y.size > 0, "Cannot display an undefined series")
      require( w > 60 && w < 1260, "Width " + w + " is out of range")
      require( h > 60 && h < 1260, "height " + h + " is out of range")
      
  	  val catDataset = new DefaultCategoryDataset
  	  y.zipWithIndex.foreach( x =>catDataset.addValue(x._1, config._2, String.valueOf(x._2)))
      draw(catDataset, w, h)
   }

   import scala.collection.immutable.List
   def display(xys: List[(DblVector, String)], w: Int, h: Int): Unit  = {
  	  import scala.collection.JavaConversions._
  	  require( xys != null && xys.size > 0, "Cannot display an undefined series")
      require( w > 60 && w < 1260, "Width " + w + " is out of range")
      require( h > 60 && h < 1260, "height " + h + " is out of range")
  	 
      val seriesCollection = new XYSeriesCollection
  	  xys.foreach( xy => {   
  	  	 val xSeries = new XYSeries(config._1 + xy._2)
  	  	 xy._1.zipWithIndex.foreach(z => xSeries.add(z._2.toDouble, z._1))
  	  	 seriesCollection.addSeries(xSeries)
  	  })

  	  val chart = ChartFactory.createXYLineChart(config._2, config._2, config._3, seriesCollection, PlotOrientation.VERTICAL, true, true, false)

  	  
  	  val plot = chart.getXYPlot
  	  plot.setBackgroundPaint(theme.paint(w, h))
  	  plot.setDomainGridlinePaint(Color.lightGray)
  	  plot.setRangeGridlinePaint(Color.lightGray)
      val xyLineRenderer: XYLineAndShapeRenderer = plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]

  	  Range(0, xys.size) foreach( n => {
  	  	xyLineRenderer.setSeriesPaint(n, colors(n>>1 % colors.size))
  	  	xyLineRenderer.setSeriesShapesVisible(n, true)
  	  	xyLineRenderer.setSeriesShape(n, shapes(n % shapes.size))
  	  	println(n)
  	  })
  	  xyLineRenderer.setSeriesLinesVisible(0, false)
  	  
  	  createFrame("First", chart)
   }
	  
   private[this] def draw(catDataset: CategoryDataset, w: Int, h: Int): Unit = {
	 val chart = ChartFactory.createLineChart(config._2, config._2, config._3, catDataset, PlotOrientation.VERTICAL, false, false, false)
	 val plot = chart.getPlot.asInstanceOf[CategoryPlot]
	    
	 val renderer = new LineAndShapeRenderer {
		 override def  getItemStroke(row: Int, col: Int): Stroke = theme.stroke(0)
	 }
	 
      plot.setRenderer(renderer)
      renderer.setSeriesShape(0, ShapeUtilities.createDiamond(1.0F))
      renderer.setSeriesPaint(0, theme.color(0))
      
      plot.setBackgroundPaint(theme.paint(w, h))
      createFrame("First", chart)
   }
}




class ScatterPlot(val _config: PlotInfo, val _theme: PlotTheme) extends Plot(_config, _theme) {
	
   override def display(xy: XYTSeries, w: Int, h : Int): Unit  = {
      require( xy != null && xy.size > 0, "Cannot display an undefined series")
      require( w > 60 && w < 1260, "Width " + w + " is out of range")
      require( h > 60 && h < 1260, "height " + h + " is out of range")
          
	  val series = new XYSeries(config._1)
	  xy.foreach( x => series.add( x._1,x._2))
	  val seriesCollection = new XYSeriesCollection
	  seriesCollection.addSeries(series)
	    
      val chart = ChartFactory.createScatterPlot(config._2, config._2, config._3, seriesCollection, PlotOrientation.VERTICAL, true, false, false)
      val plot = chart.getPlot.asInstanceOf[XYPlot]
      val renderer = new XYDotRenderer
      plot.setRenderer(renderer)
      renderer.setSeriesShape(0, ShapeUtilities.createDiamond(6.0F))
      renderer.setSeriesPaint(0, theme.color(0))
      renderer.setDotHeight(6)
      renderer.setDotWidth(6)
   
	  plot.setBackgroundPaint(theme.paint(w, h))
	  createFrame("First", chart)
    }
}



object Plot {
  type PlotInfo = (String, String, String)

}



// ------------------------  EOF ----------------------------------------------