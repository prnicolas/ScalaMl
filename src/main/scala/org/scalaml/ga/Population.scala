/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.ga

import scala.collection.mutable.ArrayBuffer
import Chromosome._
import java.util.{HashSet, Arrays}



		/**
		 * <p>Class that defines a population of chromosomes. The size of the population varies
		 * over time following successive, iterative selection but is bounded to avoid a potential
		 * explosion of candidates/chromosomes..</p>
		 * @param limit maximum number of chromosomes allowed in this populatoin
		 * @param chromosomes current pool of chromosomes
		 * @throws IllegalArgumenException if the limit is out of range or the pool of chromosomes is undefined
		 * 
		 * @author Patrick Nicolas
		 * @since August 25, 2013
		 * @note Scala for Machine Learning
		 */
import Population._
final class Population[T <: Gene](val limit: Int, 
						          val chromosomes: Pool[T]) {
    require(chromosomes != null, "Population has undefined initial set of chromosomes")
    require(limit > 1 && limit < MAX_NUM_CHROMOSOMES, "Maximum number of allowed chromosomes " + limit + " is out of range")

    	/**
		 * <p>Add an array of chromosomes to the existing population. The new chromosomes are
		 * appended to the existing ones, after removal of duplicates</p>
		 * @param xyList array of chromosomes to be added to the existing population
		 * @throws IllegalArgumentException if the array of chromosomes is undefined.
		 */
	def + (that: Population[T]): Population[T] = {
	    require(that != null, "Cannot add an undefined list of chromosomes to this population")
	    
	    if(that.size > 0) Population[T](limit, chromosomes ++: that.chromosomes) else this
	}
    

    def += (that: List[T]): Unit = {
       require(that != null, "Cannot add an undefined chromosome to this population")
       require(that.size == chromosomes(0).size, "The number of genes " + that.size + " is inconsistent the chromosome size " + chromosomes(0).size)
       chromosomes += new Chromosome[T](that)
    }
  
    
    
    
	
    	/**
    	 * <p>Selection operator for the chromosomes pool The selection relies on the
    	 * normalized cumulative fitness for each of the chromosome ranked by decreasing
    	 * order.</p>
    	 * @param cutOff cutoff ratio ]0, 1[ used in the selection
    	 * @param fit function used in the selection 
    	 * @return a new population
    	 * @throws IllegalArgumenException if the cutoff is out of bounds or the fitness function is undefined
    	 */
    def select(cutOff: Double, fit: Chromosome[T] => Double): Population[T] = {
    	require(cutOff > 0.0 && cutOff < 1.0, "cannot select chromosomes with a cutoff" + cutOff + " out of range")
    	require(fit != null, "Cannot select chromosomes in a population with undefined fitness function")
    	
    	val cumul = chromosomes.foldLeft(0.0)((sum, x) => sum + fit(x) )
    	chromosomes foreach( _ /= cumul)
        chromosomes.sortWith(_.fitness < _.fitness)
            
        var sum = 0.0
    	chromosomes.foreach( x => {sum += x.fitness; x.fitness = sum} )
    	
	    val idx = binSearch(cutOff)
	    val nSize = if(idx < limit) idx else limit
        Population[T](limit, chromosomes.take(nSize))
    }
    
    
    	/**
    	 * <p>Applies the cross-over operator on the population by pairing
    	 * the half most fit chromosomes with the half least fit chromosomes.</p>
    	 * @param xOver cross-over factor [0, 1]
    	 * @return a new population with most likely a different size
    	 * @throws IllegalArgumentException if xOver is out of range.
    	 */
    def +- (xOver: Double): Population[T] = {
    	require(xOver > 0.0 && xOver < 1.0, "Cross-over factor " + xOver + " on the population is out of range")
    	  	
    	val mid = size>>1
    	val bottom = chromosomes.slice(mid,  chromosomes.size)
    	val offSprings = chromosomes.take(mid)
    	                            .zip(bottom)
    	                            .map(p => p._1 +- (p._2, xOver))
 
    	val group1 = offSprings.map( _._1)
    	val group2 =  offSprings.map( _._2)
    	Population[T](limit, chromosomes ++ group1 ++ group2)
    }
    
    
    	/**
    	 * <p>Apply the mutation of the population. The operation produces a duplicate set of 
    	 * chromosomes that are mutated using the mutate operator ^ on chromosome.</p>
    	 * @param mu mutation factor
    	 * @return Population with original chromosomes and mutated counter-part
    	 * @throws IllegalArgumenException if mu is out of range.
    	 */
    def ^ (mu: Double): Population[T] = {
        require(mu > 0.0 && mu < 1.0, "Mutation factor " + mu + " on the population is out of range")
    	Population[T](limit, chromosomes ++ chromosomes.map(_ ^ mu))
    }


    private[this] def binSearch(cutOff: Double) = {
    	var left = 0
    	var right = size-1
    	while( left <= right) {
    	  val mid = left + (right-left)>>1
    	  if( chromosomes(mid).fitness <= cutOff) 
    		 left = mid+1
    	  else 
    		 right = mid-1
    	}
    	  // We align to an even number of chromosomes because the
    	  // X-over is an operation on pair
    	if( (left & 0x01) == 0x01)
    		left += 1
    	left 
    }


    def diff(that: Population[T], depth: Int): Option[Pool[T]] = {
    	topChromosomes(depth) match {
    		case Some(top) => that.topChromosomes(depth) match {
    			case Some(thatTop) => {
    			    if( top.zip(thatTop).exists( x => x._1 != x._2 ) )
    			    	None
    			    else
    			    	Some(top)
    			 }
    			 case None => None
    		}
    		case None => None
    	}
    }

	
	def topChromosomes(depth: Int) : Option[Pool[T]] = {
	   require(depth > 0, "Cannot list a negative or null number of chromosomes:" + depth)
	   if( size > 1) 
	      Some(chromosomes.take(if(depth > size)  size else depth))
	   else 
	      None
	}
	
	
		/**
		 * <p>Retrieve the number of chromosomes in the population
		 */
	@inline
	final def size: Int = chromosomes.size
	

	override def toString: String = {
	    chromosomes.foldLeft(new StringBuilder)((buf, x) =>  { buf.append(x.toString); buf.append("\n")}).toString
	}
}


		/**
		 * Companion object for the Population class that defines its constructor.
		 */
object Population{
   final val MAX_NUM_CHROMOSOMES = 10000
   def apply[T <: Gene](maxNumChromosomes: Int, chromosomes: Pool[T]): Population[T] = new Population[T](maxNumChromosomes, chromosomes)
}

// -----------------------  EOF -------------------------------------------