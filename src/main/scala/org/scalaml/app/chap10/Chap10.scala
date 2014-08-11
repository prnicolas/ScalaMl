/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap10

import org.scalaml.plots._



object Chap10 extends App {
//	GAEval.run
	
   
   	val theme1 = new LightPlotTheme
    val plotter1 = new LinePlot(("Predictor", "January 2014 Trading days", "Probability"), theme1)
   
    val success_ratio = Array.fill(21)(1.0, 0.0)
    		
    for( i <- 0 until success_ratio.size)
    	success_ratio(i) = (1.0, i+1)
    	
    success_ratio(2) = (0.0, 2.0)
    success_ratio(7) = (0.0, 7.0)
    success_ratio(11) = (0.0, 11.0)
    success_ratio(13) = (0.0, 13.0)
    success_ratio(14) = (0.0, 14.0)
    success_ratio(14) = (0.0, 15.0)
     
    plotter1.display(success_ratio, 300, 200)
   
    val results = Array.fill(21)(1.0)
    results(2) = 0.0
    results(7) = 0.0
    results(11) = 0.0
    results(13) = 0.0
    results(14) = 0.0
    results(15) = 0.0
    
    val actual = new Array[Double](21)
    actual(0) = -0.37
    actual(1) = -0.12
    actual(2) = -0.19
    actual(3) = 0.23
    actual(4) = -0.02
    actual(5) = -0.07
    actual(6) = 0.11
    actual(7) = -0.64
    actual(8) = 0.44
    actual(9) = 0.29
    actual(10) = -0.11
    actual(11) = -0.27
    actual(12) = 0.18
    actual(13) = 0.03
    actual(14) = -0.52
    actual(15) = -1.0
    actual(16) = -0.32
    actual(17) = 0.29
    actual(18) = -0.38
    actual(19) = 0.44
    actual(20) = -0.26
    
    
    val plotter3 = new ScatterPlot(("January 2014 S&P 500", "Normalized price change", "probability"), theme1)
    plotter3.display(actual.zip(results), 160, 140)
   
   val encodingList = List()
   /*
   implicit def double2Int(x: Double): Int  = x*1e+4.toInt
   implicit def intToDouble(n: Int): Double  = n*1e-4
   
   
   def _fitness(trades: Chromosome[Signal], values: DblVector, delta: Double): Double =  {
  	   val signals = trades.code.zipWithIndex.map(g => {
  	      val valOp = g._1.decode(intToDouble)
  	      Signal(String.valueOf(g._2), valOp._1, valOp._2)
  	   })
  	   signals.zip(values).foldLeft(0.0) ((fit, s) => fit + s._1.score(s._2, delta))
   }
   * 
   */

}


// ----------------------------------  EOF ----------------------------------------------------------------