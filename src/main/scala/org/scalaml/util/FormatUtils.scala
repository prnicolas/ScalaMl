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
package org.scalaml.util

import org.scalaml.core.Types.ScalaMl._


		/**
		 * Singleton that defines the different format to display result of computation using
		 * string format and conversion for rounding errors
		 * 
		 * @author Patrick Nicolas
		 * @since May 15, 2014
		 * @note Scala for Machine Learning
		 */
object FormatUtils {
	import java.text.DecimalFormat
	
	private val NO_ROUNDING_ERROR = -1.0
	private val ROUNDING_ERROR = 1e-5
	private val ONE_ROUNDING_ERROR = 1.0 - ROUNDING_ERROR
	
		/**
		 * Class to format the output of double floating point, integer and string values. 
		 * The client has the option to specify whether the value has to be rounded to the next Int.
		 * If the rounding error is 0.001  3.9999 will be rounded to 4 and 6.0003 will be rounded to 6
		 * 
		 * @constructor Create a format type for floating point values
		 * @param align defines the alignment for the display of the value (i.e. '%6s')
		 * @param fmtStr for Decimal values (i.e. '#,##0.000')
		 * @param roundingError Specify the maximum adjustment for rounding error. Default No rounding
		 * error
		 */
	protected class FormatType(
			align: String, 
			fmtStr: String, 
			roundingError: Double = NO_ROUNDING_ERROR) {	
		val fmt = new DecimalFormat(fmtStr)
	
			/**
			 * Format a floating point value
			 * @param x floating point value
			 */
		def toString(x: Double): String = s"${String.format(align, fmt.format(conv(x)))}"
		
			/**
			 * Format an integer value
			 * @param n integer
			 */
		def toString(n: Int): String = String.format(align, n.toString)
			/**
			 * Format a string
			 * @param s characters string
			 */
		def toString(s: String): String =  String.format(align, s)
			/**
			 * Format a parameterized type T
			 * @param t value of type T to format
			 */
		def toString[T](t: T): String = String.format(align, t.toString)
		
		
			/*
			 * Applies a rounding error scheme is the 
			 */
		private def conv(x: Double): Double = roundingError match {
			case NO_ROUNDING_ERROR => x
			case ROUNDING_ERROR =>
				val xFloor = x.floor
				if(x - xFloor < ROUNDING_ERROR) xFloor 
				else if(x - xFloor > ONE_ROUNDING_ERROR) xFloor+1.0 
				else x
		}
	}
		
		/**
		 * Short format as 6.004912491 => 6.004
		 */
	final object SHORT extends FormatType("%8s", "#,##0.000")
		/**
		 * Short format with rounding error adjustment as 6.0049124 => 6.000
		 */
	final object SHORT_ROUND extends FormatType("%8s", "#,##0.000", ROUNDING_ERROR)
		/**
		 * Medium format as 6.004912491 => 6.00491
		 */
	final object MEDIUM extends FormatType("%11s", "#,##0.00000")
			/**
		 * Medium format as 6.004912491 => 6.004913491
		 */
	final object LONG extends FormatType("%15s", "#,##0.00000000")
	
		/**
		 * Method to format a time series (x,y) tuples with labels for each axis,
		 * a formatter and data point labels is provided
		 * @param xy time series {x, y}
		 * @param xLabel label or legend for the x-Axis
		 * @param yLabel label or legend for the y-Axis
		 * @param fmt Format type used in the representation of the time series values
		 * @param labels  Labels for each of the data points of the time series
		 */
	def format(
			xy: Vector[DblPair], 
			xLabel: String, 
			yLabel: String, 
			fmt: FormatType, 
			labels: Array[String] = Array.empty): String = {
	  
		require( xy.nonEmpty, "FormatUtils.format XYTSeries is undefined")

		val labelsHeader = s"$xLabel\t$yLabel"
		val content = if( labels.isEmpty )
			xy.map { case(x, y) => s"${fmt.toString(x)}${fmt.toString(y)}"}.mkString("\n")
		else {
			assert(xy.size == labels.length,
					s"FormatUtils.toString data size ${xy.size} != number of labels ${labels.length}")
			xy.zip(labels).map{ case(z, lbl) => 
				s"$lbl${fmt.toString(z._1)}${fmt.toString(z._2)}" }.mkString("\n")
		}
					
		s"$labelsHeader$content"
	}

		
		/**
		 * Method to format a time series of single variable using a given format
		 * @param x time series of single variable of parameterized type T
		 * @param label for y-Axis or values
		 * @param fmt Format type used in the representation of the time series values
		 */
	def format[T](x: Vector[T], label: String, fmt: FormatType): String = {
		require( x.nonEmpty, "FormatUtils.format Array of type T is undefined")

		val content = x.view.zipWithIndex.map{ case(_x, n) => s"$n ${fmt.toString(_x)}"}.mkString("\n")
		if( label.nonEmpty )
			s"${fmt.toString(label)}\n$content"
		else
			content
	}
	
	
  def format[T](x: Array[T], label: String, fmt: FormatType): String = 
  	format(x.toVector, label, fmt)

		
			/**
		 * Method to format a vector or array with a short format
		 * @param x vector or single variable time seriesT
		 */
	def format(x: DblVector): String = {
		require( x.nonEmpty, "FormatUtils.format Vector of double is undefined")
	  
		x.view.zipWithIndex.map { case (_x, n) => s"${_x}  ${SHORT.toString(n)}"}.mkString(" ")
	}
	
	def format(x: DblArray): String = format(x.toArray)

		/**
		 * Method to format a single floating point value using a given format
		 * @param x value of type Double
		 * @param label for y-Axis or values
		 * @param fmt Format type used in the representation of the time series values
		 */
	def format(x: Double, label: String, fmt: FormatType): String = 
			if(label.length >1) s"$label ${fmt.toString(x)}" else fmt.toString(x)
		
			/**
		 * Method to format a matrix (Array[Array[Double]]) given a format type
		 * @param m Matrix of Double values
		 * @param fmt Format type used in the representation of the time series values
		 */
	def format(m: DblMatrix, fmt: FormatType): String = {
		require( !m.isEmpty, "FormatUtils.format Matrix is undefined")

		val header = m.head.indices.map(n => s"${fmt.toString(n)}").mkString
		val content = m.indices.map(i => s"${fmt.toString(i)} ${ m.head.indices.map(j
				=> s"${fmt.toString(m(i)(j))}").mkString }").mkString("\n")
		s"$header\n$content"
	}
	
			/**
		 * Textual representation of a vector with and without the element index
		 * @param v vector to represent
		 * @param index flag to display the index of the element along its value. Shown if index is 
		 * true, not shown otherwise
		 */
	def toText(v: DblArray, index: Boolean): String = {
		require( !v.isEmpty, 
						"ScalaMl.toText Cannot create a textual representation of a undefined vector")
		if( index)
			v.view.zipWithIndex.map{ case(u, _v) => s"${_v}:$u"}.mkString(", ")
		else
			v.view.dropRight(1).mkString(", ")
	}

		/**
		 * Textual representation of a matrix with and without the element index
		 * @param m matrix to represent
		 * @param index flag to display the index of the elements along their value. Shown if 
		 * index is true, not shown otherwise
		 */
	def toText(m: DblMatrix, index: Boolean): String = {
		require( !m.isEmpty, 
				"ScalaMl.toText Cannot create a textual representation of a undefined vector")

		if(index)
			m.view.zipWithIndex.map { case (u, v) => s"$v:${toText(u, index)}" }.mkString("\n")
		else 
			m.view.map(v => s"${toText(v, index)}").mkString("\n")
	}
}

// --------------------------  EOF --------------------------------