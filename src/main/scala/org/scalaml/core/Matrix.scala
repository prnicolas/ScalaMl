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
package org.scalaml.core

import scala.reflect.ClassTag
import scala.util.Random
import scala.annotation.implicitNotFound

import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.util.FormatUtils


		/**
		 * <p>Class that contains basic Matrix manipulation methods. This class is used to simplify some
		 * operations in hidden Markov model and Reinforcement learning algorithms.<br>
		 *  @constructor Create a matrix with a given number of rows, columns and optional content
		 *  @param nRows Number of rows in this matrix
		 *  @param nCols Number of columns in this matrix
		 *  @param data Content of this matrix, flatten as an array which size should be nRows*nCols.
		 *  @throws IllegalArgumentException if the parameters are out of bounds
		 *  @throws ImplicitNotFoundException if the conversion from T to Double is undefined prior 
		 *  the instantiation of a matrix
		 *  
		 *  @author Patrick Nicolas
		 *  @since February 23, 2014
		 *  @note Scala for Machine Learning
		 */
@implicitNotFound("Matrix  Conversion from type T to Double is undefined")
final class Matrix[@specialized(Double, Int) T: ClassTag](
		val nRows: Int, 
		val nCols: Int, 
		val data: Array[T])
		(implicit f: T => Double) {
	import Matrix._
	check(nRows, nCols, data)
	
		/**
		 * Returns the elements of the matrix at row i and column j
		 * @param i Row index for the element to return
		 * @param j Column index for the element to return
		 * @return element(i)(j) if the row and column indices are not out of bounds
		 * @throws IllegalArgumentException if either the row index or the col index are out of bounds
		 */
	final def apply(i: Int, j: Int): T = {
		require(i < nRows, s"Matrix.apply Row index $i is out of bounds")
		require(j < nCols, s"Matrix.apply Column index $j is out of bounds")
		data(i*nCols+j)
	}

		/**
		 * <p>Compute the difference between two matrices using a distance method. The distance 
		 * method sums of the distances between all the elements of each matrix (i.e. Least square 
		 * error).</p>
		 * @param that The matrix to compare to
		 * @param compare method that compute the difference between corresponding elemets in 
		 * two matrices
		 * @return Sum of the difference
		 */
	final def diff(that: Matrix[T], distance: (T, T) => Double)(implicit f: T => Double): Double = {
		require(nRows == that.nRows, 
				s"Matrix.diff Matrices have different number of rows: $nRows and ${that.nRows}")
		require(nCols == that.nCols, 
				s"Matrix.diff Matrices have different number of cols: $nCols and ${that.nCols}")

		data.zip(that.data).foldLeft(0.0)((s, xy) => s + distance(xy._1, xy._2))
	}

		/**
		 * Extract the content of a row for this matrix at a given row index
		 * @param iRow index of the row to extract from the matrix
		 * @return row at index i if index is in bounds
		 * @throws IllegalArgumentException if the row index iRow exceeds the number of rows, nRows
		 */
	final def row(iRow: Int): Array[T] = { 
		require(iRow >= 0 & iRow < nRows, s"Matrix.cols The row index $iRow is out of bounds ")
		val idx = iRow*nCols
		data.slice(idx, idx + nCols) 
	}
	
		/**
		 * Extract the content of a column for this matrix at a given row index
		 * @param iCol index of the column to extract from the matrix
		 * @return column at index (?, iCol) if index is in bounds
		 * @throws IllegalArgumentException if the column index iCol exceeds the number of columns, nCols
		 */
	final def col(iCol: Int): Array[T] = { 
		require(iCol >= 0 & iCol < nCols, s"Matrix.cols The column index $iCol is out of bounds ")
		(iCol until data.size by nCols).map(data(_)).toArray
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
		 * @return new matrix transposed from this matrix
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
   

		/**
		 * Formatted textual representation of the matrix with rows and column indices.
		 * The matrix is presented as a table of rows
		 */
	override def toString: String = 
		data.zipWithIndex.map(x => { 
			val x_str = FormatUtils.format(x._1, "", FormatUtils.ShortFormat)
			if(x._2 % nCols == 0) s"$x_str\n" else s"$x_str,"
		}).mkString
}


		/**
		 * Companion object to the class Matrix. This singleton defines
		 * the Matrix constructor and validate its parameters
		 * @author Patrick Nicolas
		 * @since February 23, 2014
		 *  @note Scala for Machine Learning
		 */
object Matrix {
		/**
		 * Constructor for a square matrix
		 * @param nRows number of rows and columns in this matrix
		 */
	def apply[T: ClassTag](nRows: Int)(implicit f: T => Double): Matrix[T] = apply(nRows, nRows)
	
	
		/**
		 * Constructor for Matrix given an array of data and its dimension (rows, columns)
		 * @param nRows Number of rows in the matrix
		 * @param nCols Number of columns in the matrix
		 * @param data Content (elements) of the matrix with data.size = nRows.nCols
		 */
	def apply[T: ClassTag](
			nRows: Int, 
			nCols: Int, 
			data: Array[T])(implicit f: T => Double): Matrix[T] = 
		new Matrix(nRows, nCols, data)

		/**
		 * Constructor for Matrix with null elements with a given number of rows and columns
		 * @param nRows Number of rows
		 * @param nCols number of columns
		 */
	def apply[T: ClassTag](nRows: Int, nCols: Int)(implicit f: T => Double): Matrix[T] = 
		new Matrix(nRows, nCols, new Array[T](nCols*nRows))
	
		/**
		 * Constructor for Matrix given an array of array of elements
		 * @param xy Array of Array of elements
		 */
	def apply[T: ClassTag](xy: Array[Array[T]])(implicit f: T => Double): Matrix[T] = 
		new Matrix(xy.size, xy(0).size, xy.flatten)
   
	final val MAX_NUM_ROWS = 8192
	final val MAX_NUM_COLS = 8192
	
	private def check[T](nRows: Int, nCols: Int, data: Array[T]): Unit = {
		require(nRows > 0 && nRows < MAX_NUM_ROWS, 
				s"Matrix.check Number of rows $nRows is out of range")
		require(nCols > 0 && nCols < MAX_NUM_COLS, 
				s"Matrix.check Number of rows $nCols is out of range")
		require( !data.isEmpty, "Matrix.check Data in undefined")
	}
	
		/**
		 * Create an empty Matrix
		 */
	def empty[T: ClassTag](implicit f: T => Double): Matrix[T] = new Matrix[T](0, 0,  Array.empty[T])
}

// ------------------------------------  EOF ---------------------------------------------