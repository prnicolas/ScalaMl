/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95d
 */
package org.scalaml.ga

import java.util.BitSet
import scala.annotation.implicitNotFound
import org.scalaml.core.types.ScalaMl.DblVector


trait Operator {
  def id: Int
  def apply(idx: Int): Operator
}

object NO_OPERATOR extends Operator {
  override def id: Int = -1
  def apply(idx: Int): Operator = NO_OPERATOR
}


import Gene._

		/**
		 * <p>Create a conversion class between discrete time series and continuous time series.</p>
		 * @constructor Discretization class that convert a Double to Int and an Int to a Double. [toInt] Function which discretizes a continuous signal or pseudo-continuous data set. [toDouble] Function that converts a discretized time series back to its original values
		 * @param toInt function which discretizes a continuous signal or pseudo-continuous data set
		 * @param toDouble convert a discretized time series back to its original values
		 */
case class Discretization(toInt: Double => Int, toDouble: Int => Double) {
   def this(R: Int) = this((x: Double) => (x*R).floor.toInt, (n: Int) => n/R)
}

 

	/**
	 * <p>Implementation of a gene as a tuple (value, operator) for example the
	 * of tje rule IF( _ operator value) THEN action.
	 * (i.e. IF (RSI > 0.8 THEN Stock over-bought ). The gene has a fixed size
	 * of bits with in this case, two bits allocated to the operator and 
	 * 32 bits allocated to the value. The floating point value(min, max) is
	 * digitized as integer [0, 2^32-1]. The discretization function is provided
	 * implicitly. The bits are implemented by the Java BitSet class.</p>
	 * @constructor Create a gene instance. [value] Floating point value to be digitized as integer, [op] Logical operator of type Operator, [discr]: Implicit discretization function from Floating point value to integer
	 * @param value Floating point value to be digitized as integer
	 * @param op logical operator of type Operator
	 * @param discr  implicit discretization function from Floating point value to integer
	 * @throws IllegalArgumentException if operator is undefined
	 * @throws ImplicitNotFound if the conversion from double to integer (digitize) is not provided
	 * 
	 * @author Patrick Nicolas
	 * @since August 28, 2013
	 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm/Genetic algorithm components
	 */
@implicitNotFound("Gene encoding requires double to integer conversion") 
class Gene(val id: String, val target: Double, val op: Operator)(implicit discr: Discretization) {
  require(op != null, "Cannot create a gene/predicate with undefined operator")
  require( id != null && id.length > 0, "Cannot create a signal with undefined id")
   
  		/**
  		 * Bits encoding of the tuple (value, operator) into bits. The encoding
  		 * is executed as part of the instantiation of a gene class.
  		 */
  val bits = {
  	 val bitset = new BitSet(GENE_SIZE)
     rOp foreach( i => if( ((op.id>>i) & 0x01)  == 0x01) bitset.set(i)  )
  	 rValue foreach( i => if( ((discr.toInt(target)>>i) & 0x01)  == 0x01) bitset.set(i)  )
  	 bitset
  }
  
  		/**
  		 * <p>Create a clone of this gene by duplicating its genetic material (bits).</p>
  		 * @return identical Gene
  		 */
  override def clone: Gene = 
    Range(0, bits.length).foldLeft(Gene(id, target, op))((enc, n) => { 
       if( bits.get(n)) 
         enc.bits.set(n)
       enc
     })
     
     
     	/**
     	 * <p>Generic method to compute the score of this gene. The score of the genes in a 
     	 * chromosome are summed as the score of the chromosome.
     	 * @return score of this gene
     	 */
   def score: Double = -1.0

   		
  		/**
  		 * <p>Implements the cross-over operator between this gene and another
  		 * parent gene.</p>
  		 * @param xOverIdx index of the bit to apply the cross-over
  		 * @param that other gene used in the cross-over
  		 * @return a single Gene as cross-over of two parents.
  		 * @throws IllegalArgumenException if the argument are undefined or the index is out of range
  		 */
  def +- (that: Gene, gIdx: GeneticIndices): Gene = {
  	 require(that != null, "Cannot cross over this gene with undefined gene")
       
     val clonedBits = cloneBits(bits)
     Range(gIdx.geneOpIdx, bits.size).foreach(n => 
    	if( that.bits.get(n) ) clonedBits.set(n) else clonedBits.clear(n))
    	
     val valOp = decode(clonedBits)
  	 Gene(id, valOp._1, valOp._2)
  }
  
  @inline
  final def size = GENE_SIZE
  
    	/**
  		 * <p>Implements the mutation operator on this gene</p>
  		 * @param idx genetic indexer 
  		 * @return a single mutated gene
  		 */
  def ^ (gIdx: GeneticIndices): Gene = ^ (gIdx.geneOpIdx)


      	/**
  		 * <p>Implements the mutation operator on this gene</p>
  		 * @param index index of the bit to mutate
  		 * @return a single mutated gene
  		 */
  def ^ (idx: Int): Gene = {
  	 val clonedBits = cloneBits(bits)
  	 clonedBits.flip(idx)
  	 val valOp = decode(clonedBits)
  	 Gene(id, valOp._1, valOp._2)
  }
  
  
  def decode(bits: BitSet): (Double, Operator) = 
	 (discr.toDouble(convert(rValue, bits)), op(convert(rOp, bits)) )
  

  def show: String = new StringBuilder(id).append(" ").append(op).append(" ").append(target).toString
  
  override def toString: String = 
     Range(0, bits.size).foldLeft(new StringBuilder) ((buf, n) => buf.append( (if( bits.get(n) == true) "1" else "0"))).toString
}


		/**
		 * Companion object for the Gene class to define constants and constructors.
		 */
object Gene {
    def apply(id: String, target: Double, op: Operator)(implicit discr: Discretization): Gene = new Gene(id, target, op)
      
    def cloneBits(bits: BitSet): BitSet = {
      Range(0, bits.length).foldLeft(new BitSet)((enc, n) => { 
        if( bits.get(n)) 
           enc.set(n)
        enc
      })
    }
    
   private def convert(r: Range, bits: BitSet): Int = r.foldLeft(0)((v,i) =>v + (if(bits.get(i)) (1<<i) else 0))
    
	final val VALUE_SIZE = 32
    final val OP_SIZE = 2
	final val GENE_SIZE = VALUE_SIZE + OP_SIZE
    final val rValue = Range(0, VALUE_SIZE)
	final val rOp = Range(VALUE_SIZE, VALUE_SIZE  + OP_SIZE)
}

// -------------------------------------------  EOF -------------------------------------------------