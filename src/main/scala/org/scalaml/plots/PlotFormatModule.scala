/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95d
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

import PlotFormatModule._
	/**
	 * Trait to format the plot
	 */


trait PlotFormatModule {
  val plotFormat: PlotFormat
  
  class PlotFormat(private val plotColors: PlotColors) {
    
     def draw(chart: JFreeChart): XYPlot = {
    	require( chart != null, "Cannot draw an undefined chart")
    	
        chart.setBackgroundPaint(plotColors._1);
        chart.getTitle.setPaint(Color.white)
        val plot = chart.getPlot.asInstanceOf[XYPlot]
        plot.setBackgroundPaint(plotColors._2)
        plot.setDomainGridlinePaint(plotColors._3)
        plot.setRangeGridlinePaint(plotColors._3)
        plot.setAxisOffset(new RectangleInsets(5.0, 5.0, 5.0, 5.0))
        plot.setDomainCrosshairVisible(true)
        plot.setRangeCrosshairVisible(true)
        
        plot.getDomainAxis.setTickLabelPaint(plotColors._3)
        plot.getDomainAxis.setLabelPaint(plotColors._3)
        plot.getRangeAxis.setTickLabelPaint(plotColors._3)
        plot.getRangeAxis.setLabelPaint(plotColors._3)
        
        val r = plot.getRenderer();
        if (r != null && r.isInstanceOf[XYLineAndShapeRenderer]) {
            val renderer = r.asInstanceOf[XYLineAndShapeRenderer]
            renderer.setBaseShapesVisible(false)
            renderer.setBaseShapesFilled(true)
            renderer.setDrawSeriesLineAsPath(true)
        }
        plot
    }
  }
}


object PlotFormatModule {
   type PlotColors = (Color, Color, Color)
   final val DARK_THEME = (Color.black, Color.black, Color.white)
   final val GRAY_THEME = (Color.white, Color.lightGray, Color.white)
}


// -----------------------  EOF -------------------------------------------