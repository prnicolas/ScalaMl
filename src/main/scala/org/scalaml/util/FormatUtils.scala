/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.3
 */
package org.scalaml.util

import org.scalaml.core.Types.ScalaMl.{DblMatrix, XYTSeries, XY, DblVector}


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
	
	class FormatType(align: String, fmtStr: String, roundingError: Double = NO_ROUNDING_ERROR) {	
		val fmt = new DecimalFormat(fmtStr)
		
		def toString(x: Double): String = s"${String.format(align, fmt.format(conv(x)))}"
		def toString(n: Int): String = String.format(align, n.toString)
		def toString(s: String): String =  String.format(align, s)
		def toString[T](t: T): String = String.format(align, t.toString)
		
		private def conv(x: Double): Double = roundingError match {
			case NO_ROUNDING_ERROR => x
			case ROUNDING_ERROR => {
				val xFloor = x.floor
				if(x - xFloor < ROUNDING_ERROR) xFloor 
				else if(x - xFloor > ONE_ROUNDING_ERROR) xFloor+1.0 
				else x
			}
		}
	}
	
	object ShortFormat extends FormatType("%8s", "#,##0.000")
	object ShortFormatRoundingError extends FormatType("%8s", "#,##0.000", ROUNDING_ERROR)
	object MediumFormat extends FormatType("%11s", "#,##0.00000")
	object LongFormat extends FormatType("%15s", "#,##0.00000000")
	
	def format(
			xy: XYTSeries, 
			xLabel: String, 
			yLabel: String, 
			fmt: FormatType, 
			labels: Array[String] = Array.empty): String = {
		
	  require( !xy.isEmpty, "FormatUtils.toString XYTSeries is undefined")

		val buf = new StringBuilder(s"$xLabel\t$yLabel\n")
		
	  if( labels.isEmpty )
			buf.append(xy.foldLeft(new StringBuilder)((buf, xy) => 
				buf.append(s"${fmt.toString(xy._1)}${fmt.toString(xy._2)}\n")).toString)
		else {
			assert(xy.size == labels.size, 
					s"FormatUtils.toString data size ${xy.size} != number of labels ${labels.size}")
					
			buf.append(xy.zip(labels).foldLeft(new StringBuilder)((buf, xy) => 
				buf.append(s"${fmt.toString(xy._2)}${fmt.toString(xy._1._1)}${fmt.toString(xy._1._2)}\n"))
					.toString)
		}
	  buf.toString
	}

		
	def format[T](x: Array[T], label: String, fmt: FormatType): String = {
		val buf = new StringBuilder
		if(label.size > 0)
			buf.append(s"${fmt.toString(label)}\n")
			
		buf.append(x.zipWithIndex.foldLeft(new StringBuilder)((buf, x) => 
				buf.append(s"${x._2}  ${fmt.toString(x._1)}\n")).toString)
		buf.toString
	}
		
		
	def format(x: DblVector): String = 
		x.zipWithIndex.foldLeft(new StringBuilder)((buf, x) => 
			buf.append(s"${x._2}  ${ShortFormat.toString(x._1)} ")).toString

	def format(x: Double, label: String, fmt: FormatType): String = {						
		val buf = new StringBuilder
		if(label.length > 1)
			buf.append(label)
		buf.append(s" ${fmt.toString(x)}")
		buf.toString
	}
		
		
	def format(m: DblMatrix, fmt: FormatType): String = {	
		val buf = new StringBuilder
		buf.append(Range(0, m(0).size).foldLeft(new StringBuilder)((buf, n) => 
				buf.append(s"${fmt.toString(n)}")).toString)
			
		Range(0, m.size).foreach(i => {
			buf.append(s"\n${fmt.toString(i)}")
			buf.append(Range(0, m(0).size).foldLeft(new StringBuilder)((buf, j) => 
					buf.append(s"${fmt.toString(m(i)(j))}")).toString)
		})
		buf.toString
	}
	
			/**
		 * Textual representation of a vector with and without the element index
		 * @param v vector to represent
		 * @param index flag to display the index of the element along its value. Shown if index is 
		 * true, not shown otherwise
		 */
	def toText(v: DblVector, index: Boolean): String = {
		require( !v.isEmpty, 
						"ScalaMl.toText Cannot create a textual representation of a undefined vector")
			
		if( index)
			v.zipWithIndex.foldLeft(new StringBuilder)((buf,x) => 
					buf.append(s"${x._2}:${x._1}, ")).toString
		else
			v.foldLeft(new StringBuilder)((buf, x) => 
			buf.append(s"$x,")).toString.dropRight(1)
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
			m.zipWithIndex.foldLeft(new StringBuilder)((buf, v) => 
					buf.append(s"${v._2}:${toText(v._1, true)}\n")).toString
		else 
			m.foldLeft(new StringBuilder)((buf, v) => buf.append(s"${toText(v, false)}\n")).toString
	}
}

// --------------------------  EOF --------------------------------