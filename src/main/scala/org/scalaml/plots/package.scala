/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99.1
 */
package org.scalaml

		/**
		 * This package object defines the classes that wraps the jFreeChart library with default
		 * configuration parameters. The plot supported are 
		 * 
		 * - Line plot for one or list of time series '''LinePlot'''
		 * 
		 * - Scatter plot for two dimension time series or two single variable time series
		 *  '''ScatterPlot'''
		 *  
		 * The configuration of the plots is defined in the class '''PlotTheme''' with two basic 
		 * themes:
		 * 
		 * - Plot with a black background and compatible font color '''BackPlotTheme'''
		 * 
		 * - Plot with a light grey background and compatible font color '''LightPlotTheme'''
		 * @see jFreeChart library  http://www.jfreechart.org
		 * @see Scala for Machine Learning Chapter 1: ''Getting started'' / Libraries
		 */
package object plots { }
// ---------------------------------------  EOF -----------------------------------------