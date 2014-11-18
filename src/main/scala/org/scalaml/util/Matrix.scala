/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95f
 */
package org.scalaml.util

import scala.reflect.ClassTag
import scala.util.Random
import org.scalaml.core.types.ScalaMl._
import scala.annotation.implicitNotFound


		/**
		 * <p>Class that contains basic Matrix manipulation methods<br><br>
		 * <b>nRows</b>  Number of rows in this matrix<br>
		 * <b>nCols</b>  Number of columns in this matrix<br>
		 * <b>data</b> Content of this matrix, flatten as an array which size should be nRows*nCols.</p>
		 *  @constructor Create a matrix with a given number of rows, columns and optional content
		 *  @throws IllegalArgumentException if the parameters are out of bounds
		 *  @throws ImplicitNotFoundException if the conversion from T to Double is undefined prior the instantiation of a matrix
		 *  @author Patrick Nicolas
		 *  @since Feb 23, 2014
		 *  @note Scala for Machine Learning
		 */
@implicitNotFound("Matrix  Conversion from type T to Double is undefined")
final class Matrix[@specialized(Double, Int) T: ClassTag](val nRows: Int, val nCols: Int, val data: Array[T])(implicit f: T => Double) {
	import Matrix._
	check(nRows, nCols, data)
	
		/**
		 * Returns the elements of the matrix at row i and column j
		 * @param i Row index for the element to return
		 * @param j Column index for the element to return
		 * @return element(i)(j) if the row and column indices are not out of bounds
		 * @throw IllegalArgumentException if either the row index or the col index are out of bopunds
		 */
	final def apply(i: Int, j: Int): T = {
		require(i < nRows, s"Matrix.apply Row index $i is out of bounds")
		require(j < nCols, s"Matrix.apply Column index $j is out of bounds")
		data(i*nCols+j)
	}

		/**
		 * <p>Compute the difference between two matrices using a distance method. The distance method sums
		 * of the distances between all the elements of each matrix (i.e. Least square error).</p>
		 * @param that The matrix to compare to
		 * @param compare method that compute the difference between corresponding elemets in two matrices
		 * @return Sum of the difference
		 */
	final def diff(that: Matrix[T], distance: (T, T) => Double)(implicit f: T => Double): Double = {
		require(nRows == that.nRows, s"Matrix.diff cannot compute distance between two matrices of different rows  $nRows with  ${that.nRows}")
		require(nCols == that.nCols, s"Matrix.diff cannot compare distance between two matrices Matrices of different number of cols, $nCols with ${that.nCols}")

		data.zip(that.data).foldLeft(0.0)((s, xy) => s + distance(xy._1, xy._2))
	}

		/**
		 * Extract a column for this matrix
		 * @return column at index i if index is in bounds
		 * @throws IllegalArgumentException if the index i exceeds the number of cols
		 */
	final def cols(i: Int): Array[T] = { 
		require(i >= 0 & i < nCols, s"Matrix.cols Columns index $i is out of bounds ")
		val idx = i*nRows
		data.slice(idx, idx + nRows) 
	}

		/**
		 * Divides the element of a row of index iRow by a value t
		 * @param iRow row index
		 * @param t value used as quotient to the element of the row
		 * @throws IllegalArgumentException if row index is out of bounds
		 * #throws ImplicitNotFoundException of conversion from a double to T is undefined
		 */
	@implicitNotFound("Matrix./= Conversion from Double to type T is undefined")
	def /= (iRow: Int, t: T)(implicit g: Double => T): Unit =  {
		require(iRow >= 0 & iRow < nRows, s"Matrix.cols Row index $iRow is out of bounds ")
		val i = iRow*nCols
		Range(0, nCols).foreach(k => data(i + k) /= t)
	}
	
		/**
		 * <p>Update the element at row index i and column index j</p>
		 * @param i row index of the element
		 * @param j column index of the element
		 * @param t new value of the element
		 * @throws IllegalArgumentException if either the row index or column index are out of bounds
		 */
	def += (i: Int, j : Int, t: T): Unit = {
		require(i >= 0 & i < nRows, s"Matrix.+= Row index $i is out of bounds ")
		require(j >= 0 & j < nCols, s"Matrix.+= Column index $j is out of bounds ")
		data(i*nCols +j) = t
	}
   
	def += (iRow: Int, t: T): Unit = {
		val i = iRow*nCols
		Range(0, nCols).foreach(k => data(i + k) =t)
	}

		/**
		 * <p>Compute the transpose of this matrix.</p>
		 * @return new matrix tranposed from this matrix
		 */
	def transpose: Matrix[T] = {
		val m = Matrix[T](nCols, nRows)
		Range(0, nRows).foreach(i => {
			val col = i*nCols
			Range(0, nCols).foreach(j => m += (j, i, data(col+j)))
		})
		m
	}
   
	@inline
	final def size = nRows*nCols

		/**
		 * <p>Initialize this matrix with random values [mean, mean + 1.0] with a given mean.</p>
		 * @param mean Mean value for the randomly generated value
		 * throws ImplicitNotFoundException of conversion from a double to T is undefined
		 */
	@implicitNotFound("Matrix./= Conversion from Double to type T is undefined")
	def fillRandom(mean: Double = 0.0)(implicit f: Double =>T): Unit = 
		Range(0, data.size).foreach(i => data.update(i,  mean + Random.nextDouble))
   

	override def toString: String = {
		var count = 0
		data.foldLeft(new StringBuilder)((b, x) => { 
			count += 1; 
			b.append(x).append( if( count % nCols == 0) "\n" else ",")
		}).toString
	}
}


		/**
		 * Companion object to the class Matrix. This singleton defines
		 * the Matrix constructor and validate its parameters
		 */
object Matrix {
	def apply[T: ClassTag](nRows: Int)(implicit f: T => Double): Matrix[T] = apply(nRows, nRows)
	
	def apply[T: ClassTag](nRows: Int, nCols: Int, data: Array[T])(implicit f: T => Double): Matrix[T] = 
		new Matrix(nRows, nCols, data)
	
	def apply[T: ClassTag](nRows: Int, nCols: Int)(implicit f: T => Double): Matrix[T] = 
		new Matrix(nRows, nCols, new Array[T](nCols*nRows))
	
	def apply[T: ClassTag](xy: Array[Array[T]])(implicit f: T => Double): Matrix[T] = 
		new Matrix(xy.size, xy(0).size, xy.flatten)
   
	final val MAX_NUM_ROWS = 8192
	final val MAX_NUM_COLS = 8192
	
	private def check[T](nRows: Int, nCols: Int, data: Array[T]): Unit = {
		require(nRows > 0 && nRows < MAX_NUM_ROWS, s"Matrix.check Number of rows $nRows is out of range")
		require(nCols > 0 && nCols < MAX_NUM_COLS, s"Matrix.check Number of rows $nCols is out of range")
		require(data != null, "Matrix.check Data in undefined")
	}
}

// ------------------------------------  EOF ---------------------------------------------