/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.ga

import java.util.BitSet
import scala.annotation.implicitNotFound



	/**
	 * <p>Enumerator for the Boolean operator used as component of a gene. A gene is defined
	 * as a predicate by a value and an operator.</p>
	 * @author Patrick Nicolas
	 * @since August 28, 2013
	 */
object Operator extends Enumeration  {
  type Operator = Value
  val NONE, LESS_THAN, EQUAL, GREATER_THAN = Value
}


	/**
	 * <p>Implementation of a gene as a tuple (value, operator) for example the
	 * of tje rule IF( _ operator value) THEN action.
	 * (i.e. IF (RSI > 0.8 THEN Stock over-bought ). The gene has a fixed size
	 * of bits with in this case, two bits allocated to the operator and 
	 * 32 bits allocated to the value. The floating point value(min, max) is
	 * digitized as integer [0, 2^32-1]. The discretization function is provided
	 * implicitly. The bits are implemented by the Java BitSet class.</p>
	 * @param value Floating point value to be digitized as integer
	 * @param 2-bit boolean operator of type Operator
	 * @param f  implicit discretization function from Floating point value to integer
	 * @throws IllegalArgumentException if operator is undefined
	 * @throws ImplicitNotFound if the conversion from double to integer (digitize) is not provided
	 * 
	 * @author Patrick Nicolas
	 * @since August 28, 2013
	 * @note Scala for Machine Learning
	 */
import Gene._
import Operator._

@implicitNotFound("Gene encoding requires double to integer conversion") 
class Gene(val value: Double, op: Operator)(implicit discr: Double => Int)  {
  require(op != null, "Cannot create a gene/predicate with undefined operator")
  
  		/**
  		 * Bits encoding of the tuple (value, operator) into bits. The encoding
  		 * is executed as part of the instantiation of a gene class.
  		 */
  val bits = {
  	val bitset = new BitSet(GENE_SIZE)
    rOp foreach( i => if( ((op.id>>i) & 0x01)  == 0x01) bitset.set(i)  )
  	rValue foreach( i => if( ((discr(value)>>i) & 0x01)  == 0x01) bitset.set(i)  )
  	bitset
  }
  
  override def clone: Gene = 
    Range(0, bits.length).foldLeft(Gene(value, op))((enc, n) => { 
       if( bits.get(n)) 
         enc.bits.set(n)
       enc
     })

  
     	/**
     	 * <p>Method to decode a gene as a bit sets into a tuple (value, operator).</p>\
     	 * @throws mplicitNotFound if the conversion from integer to double is not provided
     	 * @return Tuple (Value, Operator)
     	 */
  @implicitNotFound("Gene decoding requires Integer to double conversion") 
  def decode(implicit f_1: Int => Double): (Double, Operator)= {
     val value = rValue.foldLeft(0) ((valx, i) => valx + (if( bits.get(i) ) (1 << i) else 0) )
	 val opInt = rOp.foldLeft(0) ((valx, i) => valx + (if( bits.get(i) ) (1 << i) else 0) )
	 (f_1(value), Operator(opInt) )
  }

  		
  		/**
  		 * <p>Implements the cross-over operator between this gene and another
  		 * parent gene.</p>
  		 * @param index index of the bit to apply the cross-over
  		 * @param that other gene used in the cross-over
  		 * @return a single Gene as cross-over of two parents.
  		 * @throws IllegalArgumenException if the argument are undefined or the index is out of range
  		 */
  def +- (index: Int, that: Gene): Gene = {
  	 require(that != null, "Cannot cross over this gene with undefined gene")
  	 require(index >= 0 && index < GENE_SIZE, "Index " + index + " for gene cross-over is out of range")
  	 
     val newGene = clone
     Range(index, bits.size).foreach(n => if( that.bits.get(n) ) newGene.bits.set(n) else newGene.bits.clear(n))
     newGene
  }
  
    	/**
  		 * <p>Implements the mutation operator on this gene</p>
  		 * @param index index of the bit to mutate
  		 * @return a single mutated gene
  		 * @throws IllegalArgumenException if the mutation index is out of range
  		 */
  def ^ (index: Int): Unit = {
  	 require(index >= 0 && index < GENE_SIZE, "Index " + index + " for gene cross-over is out of range")
     bits.flip(index)
  }

  override def toString: String = 
     Range(0, bits.size).foldLeft(new StringBuilder) ((buf, n) => buf.append( (if( bits.get(n) == true) "1" else "0"))).toString
}


		/**
		 * Companion object for the Gene class to define constants and constructors.
		 */
object Gene {
    def apply(value: Double, op: Operator)(implicit f: Double => Int) : Gene = new Gene(value, op)(f)
    def apply(implicit f: Double => Int) : Gene = new Gene(0.0, NONE)(f)
    
	final val VALUE_SIZE = 32
    final val OP_SIZE = 2
	final val GENE_SIZE = VALUE_SIZE + OP_SIZE
    final val rValue = Range(0, VALUE_SIZE)
	final val rOp = Range(VALUE_SIZE, VALUE_SIZE  + OP_SIZE)
}

// -------------------------------------------  EOF -------------------------------------------------