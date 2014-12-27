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
package org.scalaml

		/**
		 * This package object defines the classes that wraps the jFreeChart library with default
		 * configuration parameters. The plot supported are 
		 * - Line plot for one or list of time series <b>LinePlot</b><br>
		 * - Scatter plot for two dimension time series or two single variable time series
		 *  <b>ScatterPlot</b><br>
		 * The configuration of the plots is defined in the class <b>PlotTheme</b> with two basic 
		 * themes:<br>
		 * - Plot with a black background and compatible font color <b>BackPlotTheme</b><br>
		 * - Plot with a light grey background and compatible font color <b>LightPlotTheme</b>
		 * @see jFreeChart library  http://www.jfreechart.org
		 * @note Scala for Machine Learning Chapter 1: Getting started / Libraries
		 */
package object plots { }
// ---------------------------------------  EOF -----------------------------------------