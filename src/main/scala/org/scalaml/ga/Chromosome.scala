/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.ga

import scala.annotation.implicitNotFound

			/**
			 * <p>Class that implements a parameterized chromosome using an encoding scheme and
			 * an objective/fitness function. A chromosome is a container or list of Gene that
			 * represents candidate solution to a problem or candidate model to a dataset.</p>
			 * @param code genetic code or list of Gene that is to be encoded with 0,1 bits
			 * @throws if the genetic code is undefined or empty
			 * @author Patrick Nicolas
			 * @since August 27, 2013
			 * @note Scala for Machine Learning
			 */
import Gene._
import Chromosome._
final class Chromosome[T <: Gene](val code: List[T]) {  
   require(code != null && code.size > 1, "Cannot create a chromosome from undefined genetic code")
   var fitness: Double = 1e+10

   		/**
   		 * <p>Define the cross-over operator to be applied on this chromosome. The cross-over
   		 * is hierarchical. The algorithm selects the gene associated to the cross-over index, 
   		 * swap all the genes of higher index (below or after the cross-over gene) between
   		 * the two parents and finally swap the bits within the cross-over gene.<br>
   		 * The cross over operation generates two off springs from the two original parents. The off-springs
   		 * are added to the current population along with the parents.</p>
   		 * @param that other parent chromosome
   		 * @param xOver cross-over factor [0, 1]
   		 * @throws IllegalArgumentException if the other chromosome is undefined, or have a different size 
   		 * or if the cross-over factor is out of range.
   		 * @return the pair of offspring.
   		 */
   def +- (that: Chromosome[T], xOver: Double): (Chromosome[T], Chromosome[T]) = {
       require(that != null, "Cannot cross-over this chromosome with an undefined parent")
       require(size == that.size, "Cannot cross-over chromosomes of different size")
       require(xOver > 0.0 && xOver < 1.0, "Cross factor " + xOver + " is out of range")
         
       	// Index of the gene for which the cross-over is applied
       val index = (xOver*size).floor.toInt
       val idx = if(index == 0) 1 else if (index >= size) size-1 else index
       
      	 	 // First use the global index (module the number of gene
         
       val xGenes = if(HIERARCHICAL) xGene(xOver, idx , that.code(idx) ) else (code(idx), that.code(idx))
        
       val offSprng1: List[T] = code.slice(0, idx) ::: xGenes._1 :: that.code.drop(idx+1)
       val offSprng2 = that.code.slice(0, idx) :::  xGenes._2 :: code.drop(idx+1)
       (Chromosome[T](offSprng1), Chromosome[T](offSprng2))
    }
     	
    
   		/**
   		 * <p>Mutation operator that flip a gene selected through a mutation index.
   		 * The mutated gene is added to the population (gene pool).</p>
   		 * @param mu mutation factor [0, 1]
   		 * @throws IllegalArgumentException if mu is out of range
   		 * @return a mutated chromosome
   		 */
    def ^ (mu: Double): Chromosome[T] = {         
    	require( mu > 0.0 && mu < 1.0, "Cannot mutate with factor " + mu + " out of range")
    	       
    	val index = (mu*size).floor.toInt
        val idx = if(index == 0) 1 else if (index >= size) size-1 else index
        
        	// Get the mutation index in the gene to mutate
        val mutated = clone
        mutated.code(idx) ^ (mu*GENE_SIZE).floor.toInt
        mutated
    }
     
     
    	/**
    	 * <p>Normalize the fitness of this chromosome with a factor. This 
    	 * operation is required by the selection algorithm.</p>
    	 * @param normalizedFactor normalization factor
    	 * @throws IllegalArgumentException if the normalization factor is less than EPS
    	 */
    def /= (normalizeFactor: Double): Unit = {
    	require( normalizeFactor > Chromosome.EPS, "Cannot normalize with " + normalizeFactor)
    	fitness /= normalizeFactor
    }

    
    @inline
    def == (that: Chromosome[T]): Boolean = if( that == null) false else code == that.code
     
    @inline
    def compareTo(thatCode: Gene): Boolean = if(thatCode == null) false else (code == thatCode)
     
    @inline
    @implicitNotFound("Conversion from Gene to parameterized type undefined in decoding chromosome")
    def decode(implicit d: Gene => T): List[T] = code.map( d(_))
     
     
    override def clone: Chromosome[T] = Chromosome[T](code)
    
    @inline
    final def size: Int = code.size
    
    override def toString: String = String.valueOf(code.toString)
     
    private[this] def xGene(xOver: Double, idx: Int, thatCode: T): (T, T) = {
        val cdx = (xOver*GENE_SIZE).floor.toInt
        ((code(idx) +- (cdx, thatCode)).asInstanceOf[T], (thatCode +- (cdx, code(idx))).asInstanceOf[T] )
    }
}


	/**
	 * Companion object to a Chromosome used to define the constructors
	 * @author Patrick Nicolas
	 * @since September 2, 2013
	 */
object Chromosome {
  import scala.collection.mutable.ListBuffer
  
  final val EPS = 1e-10
  final val HIERARCHICAL = true
  def apply[T <: Gene](code: List[T]): Chromosome[T] = new Chromosome[T](code)
  def apply[T <: Gene](predicates: List[T], encode: T => Gene): Chromosome[T] = new Chromosome[T](if(predicates.size == 1) 
                    List[T](encode(predicates(0)).asInstanceOf[T])
                 else 
                    predicates.foldLeft(List[T]()) ((xs, t) => encode(t).asInstanceOf[T] :: xs))
  type Pool[T <: Gene] = ListBuffer[Chromosome[T]]
}
      



// ------------------------  EOF --------------------------------------