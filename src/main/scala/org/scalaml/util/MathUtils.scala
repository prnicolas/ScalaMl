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
 * Version 0.99
 */
package org.scalaml.util

import scala.reflect.ClassTag
import scala.util.Random
import scala.annotation.implicitNotFound

import org.scalaml.core.Types._
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.util.FormatUtils._



	/**
	 * Object that encapsulates some mathematical primitives such as matrix, 
	 */
object MathUtils {
			/**
			 * Class that contains basic Matrix manipulation methods. This class is used to simplify some
			 * operations in '''hidden Markov models''' and '''Reinforcement learning''' algorithms.
			 *  @constructor Create a matrix with a given number of rows, columns and optional content
			 *  @tparam type of element in the matrix
			 *  @param nRows Number of rows in this matrix
			 *  @param nCols Number of columns in this matrix
			 *  @param data Content of this matrix, flatten as an array which size should be nRows*nCols.
			 *  @throws IllegalArgumentException if the parameters are out of bounds
			 *  @throws ImplicitNotFoundException if the conversion from T to Double is undefined prior 
			 *  the instantiation of a matrix
			 *  
			 *  @author Patrick Nicolas
			 *  @since 0.98.1 February 23, 2014
			 *  @version 0.99
			 *  @see Scala for Machine Learning
			 */
	@throws(classOf[IllegalArgumentException])
	@implicitNotFound(msg = "DMatrix  Conversion from type $T to Double is undefined")
	class DMatrix(
			val nRows: Int, 
			val nCols: Int, 
			val data: DblArray) {
	  
		import DMatrix._
		check(nRows, nCols, data)
		
			/**
			 * Returns the elements of the matrix at row i and column j
			 * @param i Row index for the element to return
			 * @param j Column index for the element to return
			 * @return element(i)(j) if the row and column indices are not out of bounds
			 * @throws IllegalArgumentException if either the row index or the col index are out of bounds
			 */
		@throws(classOf[IllegalArgumentException])
		final def apply(i: Int, j: Int): Double = {
			require(i < nRows, s"Matrix.apply Row index $i should be < $nRows")
			require(j < nCols, s"Matrix.apply Column index $j should be < $nCols")
			data(i*nCols+j)
		}
	
			/**
			 * Compute the difference between two matrices using a distance method. The distance 
			 * method sums of the distances between all the elements of each matrix (i.e. Least square 
			 * error).
			 * @param that The matrix to compare to
			 * @param compare method that compute the difference between corresponding elements in 
			 * two matrices
			 * @return Sum of the difference
			 */
		@throws(classOf[IllegalArgumentException])
		final def diff(that: DMatrix, distance: DblPair => Double): Double = {
			require(nRows == that.nRows, 
					s"Matrix.diff Matrices have different number of rows: $nRows and ${that.nRows}")
			require(nCols == that.nCols, 
					s"Matrix.diff Matrices have different number of cols: $nCols and ${that.nCols}")
	
			data.zip(that.data)./:(0.0)((s, xy) => s + distance(xy._1, xy._2))
		}
	
			/**
			 * Extract the content of a row for this matrix at a given row index
			 * @param iRow index of the row to extract from the matrix
			 * @return row at index i if index is in bounds
			 * @throws IllegalArgumentException if the row index iRow exceeds the number of rows, nRows
			 */
		@throws(classOf[IllegalArgumentException])
		final def row(iRow: Int): DblArray = { 
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
		@throws(classOf[IllegalArgumentException])
		final def col(iCol: Int): IndexedSeq[Double] = { 
			require(iCol >= 0 & iCol < nCols, s"Matrix.cols The column index $iCol is out of bounds ")
			
			(iCol until data.size by nCols).map( data(_) )
		}
	
			/**
			 * Divides the element of a row of index iRow by a value t
			 * @param iRow row index
			 * @param t value used as quotient to the element of the row
			 * @throws IllegalArgumentException if row index is out of bounds
			 * @throws ImplicitNotFoundException of conversion from a double to T is undefined
			 */
		@throws(classOf[IllegalArgumentException])
		def /= (iRow: Int, t: Double): this.type =  {
			require(iRow >= 0 & iRow < nRows, s"Matrix.cols Row index $iRow is out of bounds ")
			
			val i = iRow*nCols		
			Range(0, nCols).foreach(k => {
				val old = data(i+k)
				data.update(i + k, old/t) 
			})
			this
		}
		
			/**
			 * Update the element at row index i and column index j
			 * @param i row index of the element
			 * @param j column index of the element
			 * @param t new value of the element
			 * @throws IllegalArgumentException if either the row index or column index are out of bounds
			 */
		def += (i: Int, j : Int, t: Double): this.type = {
			require(i >= 0 & i < nRows, s"Matrix.+= Row index $i is out of bounds ")
			require(j >= 0 & j < nCols, s"Matrix.+= Column index $j is out of bounds ")
			
			val index = i*nCols +j
			data.update(index, data(index)+ t)
			this
		}
		
		def update(i: Int, j : Int, t: Double): this.type = {
			require(i >= 0 & i < nRows, s"Matrix.+= Row index $i is out of bounds ")
			require(j >= 0 & j < nCols, s"Matrix.+= Column index $j is out of bounds ")
			data.update(i*nCols +j, t)
			this
		}
	
		
				/**
			 * Update the element at row index i and column index j
			 * @param i row index of the element
			 * @param j column index of the element
			 * @param t new value of the element
			 * @throws IllegalArgumentException if either the row index or column index are out of bounds
			 */
		@throws(classOf[IllegalArgumentException])
		def + (i: Int, j : Int, t: Double): DMatrix = {
			require(i >= 0 & i < nRows, s"Matrix.+= Row index -und $i is out of bounds ")
			require(j >= 0 & j < nCols, s"Matrix.+= Column index $j is out of bounds ")
			
			val newData = data.clone
			val index = i*nCols +j
			newData.update(index, data(index) + t)
			new DMatrix(nRows,nCols, newData)
		}
	   
			
			/**
			 * Method to update a row of index '''iRow''' with a single value '''t'''
			 * @param iRow index of the row of the matrix to update
			 * @param t single value used to update the entire row
			 */
		@throws(classOf[IllegalArgumentException])
		def += (iRow: Int, t: Double): Unit = {
		  require(iRow >= 0 & iRow < nRows, s"Matrix.+= row index $iRow is out of bounds ")
		  
			val i = iRow*nCols
			
			Range(0, nCols).foreach(k => {
				val old = data(i+k)
				data.update(i + k, old + t) 
			})
		}
	
		@throws(classOf[IllegalArgumentException])
		def update(iRow: Int, t: Double): Unit = {
		  require(iRow >= 0 & iRow < nRows, s"Matrix.+= row index $iRow is out of bounds ")
			val i = iRow*nCols
			Range(0, nCols).foreach(k => data.update(i + k, t)  )
		}
			/**
			 * Compute the transpose of this matrix.
			 * @return new matrix transposed from this matrix
			 */
		def transpose: DMatrix = {
			val m = DMatrix(nCols, nRows)
			
			Range(0, nRows).foreach(i => {
				val col = i*nCols
				Range(0, nCols).foreach(j => m += (j, i, data(col+j)))
			})
			m
		}
	
		  /**
		   * Return the total number of elements in the matrix
		   */
		@inline
		final def size = nRows*nCols
	
			/**
			 * Method that normalizes the rows of a matrix so the sum of values is 1.0. This method
			 * is not immutable
			 * @param g implicit conversion from double to T
			 * @throws ImplicitNotFoundException of conversion from a double to T is undefined
			 */
		@implicitNotFound("Matrix.normalizeRows Conversion Double => [T] undefined")
		def normalizeRows: Unit =
			Range(0, nRows).foreach(iRow => {
				val idx = iRow*nCols
				val dataRow: DblArray = data.slice(idx, idx + nCols)
				val sum = dataRow.sum
				Range(0, nCols).foreach(j => data.update(iRow*nCols + j, dataRow(j).toDouble/sum) )
			})
			
			
		def diagonal: IndexedSeq[Double] = (0 until data.size by nCols+1).map( data(_))
		
		def trace: Double = diagonal.sum
		
		def toArrayArray: DblMatrix = 
			(0 until data.size by nCols).map(n => data.slice(n, n+nCols)).toArray
		
			/**
			 * Formatted textual representation of the matrix with rows and column indices.
			 * The matrix is presented as a table of rows
			 */
		override def toString: String = 
			Range(0, nRows).map(n => s"$n: ${row(n).mkString(", ")}").mkString("\n")
	}
	
	
			/**
			 * Companion object to the class Matrix. This singleton defines
			 * the Matrix constructor and validate its parameters
			 * @author Patrick Nicolas
			 * @since February 23, 2014 0.98.2
			 * @version 0.98.2
			 * @see Scala for Machine Learning
			 */
	object DMatrix {
			/**
			 * Constructor for a square matrix
			 * @param nRows number of rows and columns in this matrix
			 */
		def apply(nRows: Int): DMatrix = apply(nRows, nRows)
		
			/**
			 * Constructor for Matrix given an array of data and its dimension (rows, columns)
			 * @param nRows Number of rows in the matrix
			 * @param nCols Number of columns in the matrix
			 * @param data Content (elements) of the matrix with data.size = nRows.nCols
			 */
		def apply(
				nRows: Int, 
				nCols: Int, 
				data: DblArray): DMatrix = 
			new DMatrix(nRows, nCols, data)
	
			/**
			 * Constructor for Matrix with null elements with a given number of rows and columns
			 * @param nRows Number of rows
			 * @param nCols number of columns
			 */
		def apply(nRows: Int, nCols: Int): DMatrix = 
			new DMatrix(nRows, nCols, Array.ofDim[Double](nCols*nRows))
		
			/**
			 * Constructor for Matrix given an array of array of elements
			 * @param xy Array of Array of elements
			 */
		def apply(xy: DblMatrix): DMatrix = 
			new DMatrix(xy.size, xy(0).size, xy.flatten)
		
		
			/**
			 * Constructor of a matrix with a random value ranging from [mean, mean+1.0]
			 * This method applies only to matrix of type double.
			 * @param nRows Number of rows in the matrix
			 * @param nCols Number of columns in the matrix
			 * @param mean lower band of the interval of size 1.0 for which the random values are generated
			 */
		def apply(nRows: Int, nCols: Int, mean: Double): DMatrix = {
			val data = Array.tabulate(nCols*nRows)(_ => mean + Random.nextDouble)
			DMatrix(nRows, nCols, data)
		}
		
		
		def fill(nCols: Int)(data: Seq[DblArray]): DMatrix = {
			val matrix = 	DMatrix(nCols, data.size)
		  Range(0, nCols).foreach(i => {
				Range(0, data.size).foreach(j => matrix.update(i,j, data(i)(j)))
			})	  
			matrix
		}
	
			/**
			 * Create an empty Matrix with no rows and no columns
			 * @tparam type of element of the matrix
			 * @param f implicit conversion of parameterized type of elements of the matrix to a double
			 * @return Empty matrix
			 */
		def empty: DMatrix = DMatrix(0, 0, Array.empty[Double])
		
			/**
			 * Test if this matrix is empty
			 * @param Matrix to evaluate
			 * @return true if the matrix is empty, false otherwise
			 */
		final def isEmpty(m: DMatrix): Boolean = m.data.isEmpty
		
		
		final val MAX_NUM_ROWS = 8192
		final val MAX_NUM_COLS = 8192
		
		private def check(nRows: Int, nCols: Int, data: DblArray): Unit = {
			require(nRows > 0 && nRows < MAX_NUM_ROWS, 
					s"Matrix.check Number of rows $nRows is out of range")
			require(nCols > 0 && nCols < MAX_NUM_COLS, 
					s"Matrix.check Number of rows $nCols is out of range")
			require( data.length > 0, "Matrix.check Data in undefined")
		}
	}
	
		/**
		 * Implementation of sigmoid (or logistic function)
		 * @param x argument of the sigmoid
		 * @return value in a range of [0, 1]
		 */
	final def sigmoid(x: Double): Double = 1.0/(1.0 + Math.exp(-x))
	
	final def tanh(x: Double): Double = 2.0*sigmoid(x) - 1.0
	
	
	object Shuffle {
			/**
			 * Generation of shuffled list of integers
			 * @param n number of integers to shuffle
			 * @return Array of shuffled integers, Return an empty array of integers if n < =0
			 */
		def native(n: Int): Array[Int] = {
			import scala.collection._
			
			if( n <= 0) 
				Array.empty[Int]
			else {
				val indices = mutable.ArrayBuffer.tabulate(n)(n => n)
				Random.shuffle(indices).toArray
			}
		}
		
		import scala.util.Random
		import scala.collection._
		
		def fisherYates(n: Int): IndexedSeq[Int] = {
		  
			def fisherYates(seq: mutable.Seq[Int]): IndexedSeq[Int] = {
				require(seq.size > 0, "Undefined argument")
				Random.setSeed(System.currentTimeMillis)
				
				(0 until seq.size).map(i => {
					var randomIdx: Int = i + Random.nextInt(seq.size-i)
					seq(randomIdx) ^= seq(i)
					seq(i) = seq(randomIdx) ^ seq(i) 
					seq(randomIdx) ^= (seq(i))
					seq(i)
				})
			}
			
			if( n <= 0) 
				Array.empty[Int]
			else 
				fisherYates(mutable.ArrayBuffer.tabulate(n)(n => n))
		}
	}
}

// ------------------------------------  EOF ---------------------------------------------