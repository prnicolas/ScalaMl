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
import Types.ScalaMl._
import org.jfree.data.xy.XYDataset
import java.awt.geom.Ellipse2D
import org.jfree.chart.renderer.xy.XYShapeRenderer
import org.jfree.chart.axis.ValueAxis
import org.jfree.chart.axis.NumberAxis



object ChartType extends Enumeration {
	type ChartType = Value
	val LINE, TIME_SERIES, SCATTER, BAR = Value
}
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
          
	  val series =  xy.foldLeft(new XYSeries(config._1))((s, xy) => {s.add(xy._1, xy._2); s})
	  val seriesCollection = new XYSeriesCollection
	  seriesCollection.addSeries(series)
	  draw(seriesCollection, w, h)
   }
   
   def display(xy1: XYTSeries, xy2: XYTSeries, w: Int, h : Int): Unit  = {
  	  require( xy1 != null && xy1.size > 0 && xy2 != null && xy2.size >0, "Cannot display an undefined series")
      require( w > 60 && w < 1260, "Width " + w + " is out of range")
      require( h > 60 && h < 1260, "height " + h + " is out of range")
          
      val seriesCollection1 = new XYSeriesCollection
       val seriesCollection2 = new XYSeriesCollection
	  val series1 = xy1.foldLeft(new XYSeries(config._1))((s, xy) => {s.add(xy._1, xy._2); s})
	  val series2 = xy2.foldLeft(new XYSeries(config._1))((s, xy) => {s.add(xy._1, xy._2); s})
	  seriesCollection1.addSeries(series1)
	  seriesCollection2.addSeries(series2)
	  
	  val chart = ChartFactory.createScatterPlot(config._2, config._2, config._3, seriesCollection1, PlotOrientation.VERTICAL, true, false, false)
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
   
   
	  plot.setBackgroundPaint(theme.paint(w, h))
	  createFrame("First", chart)
   }
   
   def display(xs: scala.List[XYTSeries], lbls: scala.List[String], w: Int, h : Int): Unit  = {
  	  require(xs != null && xs.size > 0, "Cannot display an undefined series")
      require( w > 60 && w < 1260, "Width " + w + " is out of range")
      require( h > 60 && h < 1260, "height " + h + " is out of range")
          
      val seriesCollectionsList = xs.zipWithIndex.foldLeft(scala.List[XYSeriesCollection]())((xs, xy) => { 
      	  val seriesCollection = new XYSeriesCollection
      	  val series = xy._1.foldLeft(new XYSeries(lbls(xy._2)))((s, t) => {s.add(t._1, t._2); s})
      	  seriesCollection.addSeries(series)
      	  seriesCollection :: xs
      })
	  val chart = ChartFactory.createScatterPlot(config._2, config._2, config._3, seriesCollectionsList.last, PlotOrientation.VERTICAL, true, false, false)
      val plot = chart.getPlot.asInstanceOf[XYPlot]
  	  
      val renderer1 = new XYDotRenderer
      plot.setRenderer(0, renderer1)
      plot.setDomainAxis(0, new NumberAxis("Trading sessions"))
      renderer1.setSeriesPaint(0, theme.color(0))
      renderer1.setDotHeight(4)
      renderer1.setDotWidth(4)
   
      
      val shapes = Array[Shape](ShapeUtilities.createDiamond(3.0F), ShapeUtilities.createRegularCross(2.0F, 2.0F))
      seriesCollectionsList.take(seriesCollectionsList.size-1).zipWithIndex.foreach( sColi =>{ 
      	  plot.setDataset(sColi._2+1, sColi._1)
          val renderer = new XYShapeRenderer
          plot.setRenderer(sColi._2+1, renderer)
          renderer.setSeriesShape(0, shapes(sColi._2%shapes.size))
          renderer.setSeriesPaint(0, theme.color(sColi._2+1))
      })
   
	  plot.setBackgroundPaint(theme.paint(w, h))
	  createFrame("First", chart)
   }
   
   	    
    private def draw(seriesCollection: XYSeriesCollection, w: Int, h : Int) {
      val chart = ChartFactory.createScatterPlot(config._2, config._2, config._3, seriesCollection, PlotOrientation.VERTICAL, true, false, false)
      val plot = chart.getPlot.asInstanceOf[XYPlot]
      val renderer = new XYDotRenderer
      plot.setRenderer(renderer)
      renderer.setSeriesShape(0, ShapeUtilities.createDiamond(8.0F))
      renderer.setSeriesPaint(0, theme.color(3))
      renderer.setDotHeight(10)
      renderer.setDotWidth(10)
   
	  plot.setBackgroundPaint(theme.paint(w, h))
	  createFrame("First", chart)
    }
   
}



object Plot {
  type PlotInfo = (String, String, String)

}



// ------------------------  EOF ----------------------------------------------