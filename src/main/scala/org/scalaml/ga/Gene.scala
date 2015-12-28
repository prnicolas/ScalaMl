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
package org.scalaml.ga

import java.util

import scala.annotation.implicitNotFound

		/**
		 * Generic operator for symbolic representation of a gene defined
		 * as a tuple {variable, operator, target_value}. An operator can be logical (OR, AND, NOT)
		 * or numeric (>, <, ==). Symbolic operators should not be confused with
		 * genetic operators such as mutation or cross-over.
		 * 
		 * @author Patrick Nicolas
		 * @since September 12, 2013  (0.97)
		 * @version 0.98.2
		 * @see Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
trait Operator {
		/**
		 * Identifier for the operator of type Integer
		 * @return operator unique identifier
		 */
	def id: Int = -1
		
		/**
		 * Constructor for an operator. This method returns the operator associated
		 * to an identifier
		 * @param id Operator identifier
		 * @return Operator associated to this identifier
		 */
	def apply(id: Int): Operator
}

		/**
		 * Define the Null operator in the symbolic representation of a gene
		 */
object NO_OPERATOR extends Operator {
	override def id: Int = -1
	def apply(idx: Int): Operator = NO_OPERATOR
}


import org.scalaml.ga.Gene._

		/**
		 *  Class for the conversion between time series with discrete values (digital of type Int)
		 *  and time series with continuous values (analog of type Double). Continuous values 
		 *  are digitized over an interval through a linear segmentation.
		 *  
		 *  A continuous time series with minimum value, m and maximum value M is quantized over 
		 *  an interval [a, b] 
		 *  {{{
		 *  x [a, b]  x -> (x - m)*(b - a)/(M- n) + a
		 *  }}
		 * @constructor Quantization class that convert a Double to Int and an Int to a Double.
		 * @param toInt Function which discretizes a continuous signal or pseudo-continuous data set
		 * @param toDouble convert a discretized time series back to its original (continuous) values
		 * 
		 * @author Patrick Nicolas
		 * @since August 28, 2013
		 * @see Scala for Machine Learning Chapter 10 ''Genetic Algorithm''
		 */
case class Quantization(toInt: Double => Int, toDouble: Int => Double) {
	def this(R: Int) = this((x: Double) => (x*R).floor.toInt, (n: Int) => n/R)
}

 

		/**
		 * Implementation of a gene as a tuple (value, operator) for example the
		 * of the rule IF( _ operator value) THEN action.
		 * (i.e. IF (RSI > 0.8 THEN Stock over-bought ). The gene has a fixed size
		 * of bits with in this case, two bits allocated to the operator and 
		 * 32 bits allocated to the value. The floating point value(min, max) is
		 * digitized as integer [0, 2&#94;32-1]. The discretization function is provided
		 * implicitly. The bits are implemented by the Java BitSet class.
		 * @constructor Create a gene instance. 
		 * @throws IllegalArgumentException if operator or id is undefined
		 * is not provided
		 * @param id  Identifier for the Gene
		 * @param target  Target or threshold value.It is a floating point value to be digitized 
		 * as integer
		 * @param op   Symbolic operator associated to this gene
		 * @param quant  implicit discretization function from Floating point value to integer.
		 *
		 * @author Patrick Nicolas
		 * @since August 28, 2013
		 * @see Scala for Machine Learning Chapter 10 Genetic Algorithm / Genetic algorithm components
		 */
@implicitNotFound("Gene encoding requires double to integer conversion")
@implicitNotFound("Gene encoding requires quantization")
@throws(classOf[IllegalArgumentException])
class Gene(
		val id: String, 
		val target: Double, 
		val op: Operator)(implicit quant: Quantization, encoding: Encoding) {
  
	require( !id.isEmpty, "Cannot create a signal with undefined id")
   
		/**
		 * Bits encoding of the tuple (value, operator) into bits {0, 1} executed 
		 * as part of the instantiation of a gene class.
		 */
	lazy val bits = apply(target, op)

	
	def apply(value: Double, operator: Operator): util.BitSet = {
		val bitset = new java.util.BitSet(encoding.length)
			// Encode the operator
		encoding.rOp foreach(i => if( ((operator.id>>i) & 0x01)  == 0x01) bitset.set(i))
		
			// Encode the value using the quantization function
		encoding.rValue foreach(i => if( ((quant.toInt(value)>>i) & 0x01)  == 0x01) bitset.set(i)  )
		bitset	
	}
	
	def unapply(bitSet: util.BitSet): (Double, Operator) =
		(quant.toDouble(convert(encoding.rValue, bits)), op(convert(encoding.rOp, bits)))
	

	
		/**
		 * Create a clone of this gene by duplicating its genetic material (bits).
		 * @return identical Gene
		 */
	override def clone: Gene = 
		Range(0, bits.length)./:(Gene(id, target, op))((enc, n) => { 
			if( bits.get(n)) 
				enc.bits.set(n)
			enc
		})

			/**
			 * Virtual constructor for classes inherited from Gene. The virtual constructor
			 * is used by cloning, mutation and cross-over genetic operators.
			 * This method has to be overriden for each Gene sub-class.
			 * @param id Identifier for the gene
			 * @param target Target/threshold value associated to the gene
			 * @param op Operator for the input and target value '''input operator target'''
			 * @return New instance of the same gene.
			 */
	def toGene(id: String, target: Double, op: Operator) = new Gene(id,target, op)
		
		/**
		 * Generic method to compute the score of this gene. The score of the genes in a 
		 * chromosome are summed as the score of the chromosome.
		 * @return score of this gene
		 */
	def score: Double = -1.0

		/**
		 * Implements the cross-over operator between this gene and another
		 * parent gene.
		 * @param indices Genetic Index for this gene
		 * @param that other gene used in the cross-over
		 * @return A single Gene as cross-over of two parents.
		 */
	def +- (that: Gene, indices: GeneticIndices): Gene = {
		val clonedBits = cloneBits(bits)
		
		Range(indices.geneOpIdx, bits.size).foreach(n => 
			if( that.bits.get(n) )  clonedBits.set(n) else clonedBits.clear(n)
		)
		val valOp = unapply(clonedBits)
		new Gene(id, valOp._1, valOp._2)
	}

	
		/**
		 * Return the size of this gene. The size of the gene is predefined as identical
		 * for all the genes of all chromosomes in a population.
		 * @return Size of the gene
		 */
	@inline
	final def size = encoding.length
  
		/**
		 * Implements the mutation operator on this gene
		 * @param indices genetic index for the cross-over and mutation of this gene
		 * @return A mutated gene
		 */
	def ^ (indices: GeneticIndices): Gene = ^ (indices.geneOpIdx)


		/**
		 * Implements the mutation operator on this gene
		 * @param idx index of the bit to mutate (0 < idx < gene.size)
		 * @return A mutated gene
		 */
	def ^ (idx: Int): Gene = {
		val clonedBits = cloneBits(bits)
			// flip the bit
		clonedBits.flip(idx)
			// Decode or convert the bit set into a symbolic representation for the gene
		val valOp = unapply(clonedBits)
		new Gene(id, valOp._1, valOp._2)
	}
  

		/**
		 * Textual description of the symbolic representation of this gene
		 * @return description of gene id, operator and target value
		 */
	def symbolic: String = s"$id ${op.toString} $target"

		/**
		 * Textual description of the genetic representation of this gene
		 */
	override def toString: String = 
		Range(0, bits.size).map(n => if( bits.get(n)) "1" else "0").mkString("")
}


		/**
		 * Companion object for the Gene class to define constants, its constructors
		 * and duplication of genetic code
		 * @author Patrick Nicolas
		 * @since August 28, 2013 (0.97)
		 * @version 0.98.2
		 * @see Scala for Machine Learning Chapter 10 Genetic Algorithm / Genetic algorithm components
		 * 
		 */
object Gene {

		/**
		 * Default constructor for a Gene
		 * @param id  Identifier for the Gene
		 * @param target  Target or threshold value.It is a floating point value to be digitized as integer
		 * @param op   Symbolic operator associated to this gene
		 * @param quant  implicit quantization function from Floating point value to integer.
		 * @param encoding  implicit encoding function for the gene
		 */
	def apply(id: String, target: Double, op: Operator)
			(implicit quant: Quantization, encoding: Encoding): Gene = 
		new Gene(id, target, op)

	class Encoding(nValueBits: Int, nOpBits: Int) {
		val rValue = Range(0, nValueBits)
		val length = nValueBits + nOpBits
		val rOp = Range(nValueBits, length)
	}
	
	final private val VALUE_SIZE = 32
	final private val OP_SIZE = 2
	var defaultEncoding = new Encoding(VALUE_SIZE, OP_SIZE)
 	
		/**
		 * Clone the genetic code of this gene
		 * @param bits Bitset of this gene
		 * @return duplicate genetic code
		 */
	protected def cloneBits(bits: util.BitSet): util.BitSet =
		Range(0, bits.length)./:(new util.BitSet)((enc, n) => {
			if( bits.get(n)) 
				enc.set(n)
			enc
		})
 
		/*
		 * Convert a range of bits within a bit into an integer
		 */
	private def convert(r: Range, bits: util.BitSet): Int =
		r./:(0)((v,i) =>v + (if(bits.get(i)) 1<<i else 0))
}

// -------------------------------------------  EOF -------------------------------------------------