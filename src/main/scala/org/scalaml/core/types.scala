/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 * 
 * Version 0.96c
 */
package org.scalaml.core

import scala.language.implicitConversions
import java.text.DecimalFormat



		/**
		 * <p>Package that encapsulates the types and conversion used in the Scala for Machine learning
		 * project. Internal types conversion are defined by the Primitives singleton. The type conversion
		 * related to each specific libraries are defined by their respective singleton (i.e CommonsMath).</p>
		 * 
		 *  @author Patrick Nicolas
		 *  @since February 23, 2014
		 *  @note Scala for Machine Learning Chapter 3 Data pre-processing/Time series
		 */
object Types {  

	class Formatter(align: String, fmtStr: String) {
		val fmt = new DecimalFormat(fmtStr)
		
		def description(x: Double): String = s"${String.format(align, fmt.format(x))}"
		def description(n: Int): String = String.format(align, n.toString)
		def description(s: String): String =  String.format(align, s)
		def description[T](t: T): String = String.format(align, t.toString)
	}
	
	object ShortFormatter extends Formatter("%8s", "#,##0.000")
	object longFormatter extends Formatter("%16s", "#,##0.00000000")
		/**
		 * <p>Singleton that define the types and conversion between ScalaML types and native Scala types</p>
		 *  @author Patrick Nicolas
		 *  @since February 23, 2014
		 *  @note Scala for Machine Learning Chapter 3 Data pre-processing/Time series
		 */
	object ScalaMl {	  
		type XY = (Double, Double)
		type XYTSeries = Array[(Double, Double)]
 
		type DMatrix[T] = Array[Array[T]]
		type DVector[T] = Array[T]  
	  
		type DblMatrix = DMatrix[Double]
		type DblVector = Array[Double]
		

		def toString(xy: XYTSeries, xLabel: String, yLabel: String, shortFormat: Boolean, labels: Array[String] = Array.empty): String = {
			require(xy != null && xy.size > 0, "ScalaMl.toString XYTSeries is undefined")
			if(labels.size > 0)
				require(xy.size == labels.size, s"ScalaMl.toString data size ${xy.size} is difference from number of labels ${labels.size}")
			
			val fmt = if(shortFormat) ShortFormatter else longFormatter
			
			val buf = new StringBuilder(s"$xLabel\t$yLabel\n")
			if(labels.size == 0)
				buf.append(xy.foldLeft(new StringBuilder)((buf, xy) => 
					buf.append(s"${fmt.description(xy._1)}${fmt.description(xy._2)}\n")).toString)
			else 
				buf.append(xy.zip(labels).foldLeft(new StringBuilder)((buf, xy) => 
					buf.append(s"${fmt.description(xy._2)}${fmt.description(xy._1._1)}${fmt.description(xy._1._2)}\n")).toString)
			buf.toString
		}
		
		def toString[T](x: Array[T], label: String, shortFormat: Boolean): String = {
			val fmt = if(shortFormat) ShortFormatter else longFormatter
						
			val buf = new StringBuilder
			if(label.size > 0)
				buf.append(s"${fmt.description(label)}\n")
			buf.append(x.zipWithIndex.foldLeft(new StringBuilder)((buf, x) => buf.append(s"${x._2}  ${fmt.description(x._1)}\n")).toString)
			buf.toString
		}

		def toString(x: Double, label: String, shortFormat: Boolean): String = {
			val fmt = if(shortFormat) ShortFormatter else longFormatter
						
			val buf = new StringBuilder
			if(label.length > 1)
				buf.append(label)
			buf.append(s" ${fmt.description(x)}")
			buf.toString
		}
		
		
		def toString(m: Array[Array[Double]], shortFormat: Boolean): String = {	
			val fmt = if(shortFormat) ShortFormatter else longFormatter
					
			val buf = new StringBuilder
			buf.append(Range(0, m(0).size).foldLeft(new StringBuilder)((buf, n) => buf.append(s"${fmt.description(n)}")).toString)
			
			Range(0, m.size).foreach(i => {
				buf.append(s"\n${fmt.description(i)}")
				buf.append(Range(0, m(0).size).foldLeft(new StringBuilder)((buf, j) => buf.append(s"${fmt.description(m(i)(j))}")).toString)
			})
			buf.toString
		}
		
		/**
		 * <p>Generic operator on Vector of parameterized type and a DblVector</p>
		 * @param v first operand of the operation
		 * @param w second operand
		 * @param op operator/function on elements of the vectors
		 * @throws IllegalArgumentException if the operands and operator are undefined or the vectors have different length
		 */
		def Op[T <% Double](v: DVector[T], w: DblVector, op: (T, Double) => Double): DblVector = {
			require( v != null && w != null, "Cannot apply operator on undefined vectors")
			require( op != null, "Cannot apply undefined operator to vectors")
			require(v.size == w.size, s"Cannot combine vector of different size ${v.size} and ${w.size}")
	     
			v.zipWithIndex.map( x => op(x._1, w(x._2)))
		}
	  
		
		/**
		 * <p>Implementation of the dot product between a parameterized vector and
		 * a vector of double.</p>
		 * @param v first operand of the operation
		 * @param w second operand
		 * @throws IllegalArgumentException if the arguments are undefined or the vectors have different length
		 */
		def dot[T <% Double](v: DVector[T], w: DblVector): Double = {
			require( v != null && w != null, "Cannot apply dot product on undefined vectors")
			require(v.size == w.size, s"dot product requires vector of identical size ${v.size} and ${w.size}")
	     
			v.zipWithIndex.foldLeft(0.0)((s, x) => s + x._1* w(x._2) )
		}
	 
		import scala.reflect.ClassTag
		implicit def t2DVector[T: ClassTag](t: T): DVector[T] =  Array.fill(1)(t)
  	  
		implicit def int2Double(n: Int): Double = n.toDouble
		implicit def long2Double(n: Long): Double = n.toDouble
		implicit def vectorT2DblVector[T <% Double](vt: DVector[T]): DblVector = vt.map( _.toDouble)
		implicit def dblVector2String(v: DblVector): String = v.foldLeft(new StringBuilder)((s, x) => s.append(s"$x ")).toString
	  
		implicit def /(v: DblVector, n: Int): DblVector = v.map( _/n)
		implicit def /(m: DblMatrix, n: Int, z: Double) {
			require(n < m.size, s"/ matrix column $n out of bounds")
			require(Math.abs(z) > 1e-32, s"/ divide column matrix by $z too small")
			Range(0, m(n).size).foreach( m(n)(_) /= z)
		}

	  
		implicit def double2DblVector(x: Double): DblVector = Array[Double](x)
		implicit def dblPair2DbLVector(x: (Double, Double)): DblVector = Array[Double](x._1,x._2)
		implicit def dblPairs2DblRows(x: (Double, Double)): DblMatrix = Array[Array[Double]](Array[Double](x._1, x._2))
		implicit def dblPairs2DblCols(x: DblVector): DblMatrix = Array[Array[Double]](Array[Double](x(0)), Array[Double](x(1)))
		implicit def dblPairs2DblMatrix2(x: ((Double, Double), (Double, Double))): DblMatrix = 
			Array[Array[Double]](Array[Double](x._1._1, x._1._2), Array[Double](x._2._1, x._2._2))
  
		/**
		 * Textual representation of a vector with and without the element index
		 * @param v vector to represent
		 * @param index flag to display the index of the element along its value. Shown if index is true, not shown otherwise
		 */
		def toText(v: DblVector, index: Boolean): String = {
			require(v != null && v.size > 0, "ScalaMl.toText Cannot create a textual representation of a undefined vector")
			
			if( index)
				v.zipWithIndex.foldLeft(new StringBuilder)((buf, x) => buf.append(s"${x._2}:${x._1}, ")).toString
			else
				v.foldLeft(new StringBuilder)((buf, x) => buf.append(s"$x,")).toString.dropRight(1)
		}

				/**
		 * Textual representation of a matrix with and without the element index
		 * @param m matrix to represent
		 * @param index flag to display the index of the elements along their value. Shown if index is true, not shown otherwise
		 */
		def toText(m: DblMatrix, index: Boolean): String = {
			require(m != null && m.size > 0, "ScalaMl.toText Cannot create a textual representation of a undefined vector")
			
			if(index)
				m.zipWithIndex.foldLeft(new StringBuilder)((buf, v) => buf.append(s"${v._2}:${toText(v._1, true)}\n")).toString
			else 
				m.foldLeft(new StringBuilder)((buf, v) => buf.append(s"${toText(v, false)}\n")).toString
		}
	}

		/**
		 * <p>Implicit conversion from internal primitive types DblVector and DblMatrix to Apache 
		 * Commons Math types.</p>
		 * @author Patrick Nicolas
		 * @since January 23, 2014
		 * @note Scala for Machine Learning Chapter 3 Data pre-processing/Time series
		 */
	object CommonMath {
		import org.apache.commons.math3.linear._
		import ScalaMl._
  	 
		implicit def double2RealMatrix(data: DblMatrix): RealMatrix = new Array2DRowRealMatrix(data)
		implicit def double2RealMatrix2(data: DblVector): RealMatrix = new Array2DRowRealMatrix(data)
		implicit def double2RealVector(data: DblVector): RealVector = new ArrayRealVector(data)
		implicit def RealVector2Double(vec: RealVector): DblVector = vec.toArray
	}
}

// ------------------------------------------  EOF -----------------------------------------------------------