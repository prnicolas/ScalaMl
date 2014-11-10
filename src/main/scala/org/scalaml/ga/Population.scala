/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95c
 */
package org.scalaml.ga

import scala.collection.mutable.ArrayBuffer
import org.scalaml.core.types.ScalaMl.{DblVector, DblMatrix}
import Chromosome._
import java.util.{HashSet, Arrays}
import Population._
import org.scalaml.core.XTSeries
import org.scalaml.util.Display
import org.apache.log4j.Logger



		/**
		 * <p>Class that defines relative genetic index (index in the genetic code, 
		 * an operator such as mutation or cross-over acts upon.</p>
		 * @constructor Create a genetic indexer. [chOpIdx] Index of the gene in the chromosome, that is manipulated by a genetic operator. [geneOpIdx] Index of the bits in the gene that is manipulated by a genetic operator.
		 * 
		 * @author Patrick Nicolas
		 * @since June 7, 2014
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm/Genetic algorithm components
		 */
case class GeneticIndices(val chOpIdx: Int, val geneOpIdx: Int)

		/**
		 * <p>Class that defines a population of chromosomes. The size of the population varies
		 * over time following successive, iterative selection but is bounded to avoid a potential
		 * explosion of candidates/chromosomes.</p>
		 * @constructor Create a population of chromosome. [limit]: Maximum number of chromosomes allowed in this population, [chromosomes] Current pool of chromosomes
		 * @param limit maximum number of chromosomes allowed in this population
		 * @param chromosomes current pool of chromosomes
		 * @throws IllegalArgumenException if the limit is out of range or the pool of chromosomes is undefined
		 * 
		 * @author Patrick Nicolas
		 * @since August 25, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm/Genetic algorithm components
		 */
final class Population[T <: Gene](limit: Int, 
						          val chromosomes: Pool[T]) {
    require(chromosomes != null, "Population has undefined initial set of chromosomes")
    require(limit > 1 && limit < MAX_NUM_CHROMOSOMES, s"Maximum number of allowed chromosomes $limit is out of range")
    private val logger = Logger.getLogger("Population")
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
       require(that.size == chromosomes(0).size, s"The number of genes ${that.size} is inconsistent the chromosome size ${chromosomes(0).size}")
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
    def select(score: Chromosome[T]=> Unit, cutOff: Double): Unit = {
      	require(score != null, "Cannot select chromosomes in a population with undefined fitness function")
    	  	
      	val cumul = chromosomes.foldLeft(0.0)((s, xy) => {score(xy); s + xy.unfitness })
      	println(cumul + "," + chromosomes.size)
    	chromosomes foreach( _ /= cumul)
        val newChromosomes = chromosomes.sortWith(_.unfitness < _.unfitness)

        val cutOffSize: Int = (cutOff*newChromosomes.size).floor.toInt
        val newPopSize = if(limit < cutOffSize) limit else cutOffSize
        chromosomes.clear
        chromosomes ++= newChromosomes.take(newPopSize)        
    }
    
    	/**
    	 * <p>Return the size of the genes that compose the chromosomes of this population. It is assumed that 
    	 * the genes in the chromosomes have identical size.</p>
    	 * @return number of bits in the gene that compose the chromosomes of this population.
    	 */
    @inline
    final def geneSize: Int = chromosomes.head.code.head.size
    
    	/**
    	 * <p>Return the number of genes in the chromosomes of this population.</p>
    	 * @return Number of genes in each of the chromosomes of this population.
    	 */
    @inline
    final def chromosomeSize: Int = if(chromosomes.size > 0) chromosomes.head.size else 0
    
    
    	/**
    	 * <p>Applies the cross-over operator on the population by pairing
    	 * the half most fit chromosomes with the half least fit chromosomes.</p>
    	 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm/Implementation/Crossover
    	 * @param xOver cross-over factor [0, 1]
    	 * @return a new population with most likely a different size
    	 * @throws IllegalArgumentException if xOver is out of range.
    	 */
    def +- (xOver: Double): Unit = {
    	require(xOver > 0.0 && xOver < 1.0, s"Cross-over factor $xOver on the population is out of range")
    	  	
    	if( size > 1) {
	    	val mid = size>>1
	    	val bottom = chromosomes.slice(mid,  size)              
	    	val gIdx = geneticIndices(xOver)
	    	val offSprings = chromosomes.take(mid)
	    	                            .zip(bottom)
	    	                            .map(p => p._1 +- (p._2, gIdx))
	    	                            .unzip
	    	chromosomes ++= offSprings._1 ++ offSprings._2
    	}
    }
    
    
    	/**
    	 * <p>Apply the mutation of the population. The operation produces a duplicate set of 
    	 * chromosomes that are mutated using the mutate operator ^ on chromosome.</p>
    	 * @param mu mutation factor
    	 * @return Population with original chromosomes and mutated counter-part
    	 * @throws IllegalArgumenException if mu is out of range.
    	 */
    def ^ (mu: Double): Unit = {
        require(mu > 0.0 && mu < 1.0, s"Mutation factor $mu on the population is out of range")
    	chromosomes ++= chromosomes.map(_ ^ geneticIndices(mu))
    }

    def diff(that: Population[T], depth: Int): Option[Pool[T]] = {
    	fittest(depth) match {
    		case Some(top) => that.fittest(depth) match {
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

	
	def fittest(depth: Int) : Option[Pool[T]] = {
	   require(depth > 0, s"Cannot list a negative or null number of chromosomes: $depth")
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
	

	override def toString: String = 
	   chromosomes.foldLeft(new StringBuilder)((buf, x) =>  { buf.append(s"${x.toString}\n")}).toString

    def toString(comments: String): Unit = 	
	    chromosomes.foldLeft(new StringBuilder(comments))((buf, x) =>  buf.append(s"${x.toString("->")}\n")).toString

	
	private def geneticIndices(prob: Double): GeneticIndices = {
	   require(prob >= 0.0 && prob <= 1.0, s"probability for the genetic operator in genetix indexer $prob is out of range")
	   
	   var idx = (prob*chromosomeSize).floor.toInt
	   val chIdx = if(idx == 0) 1 else if(idx == chromosomeSize) chromosomeSize-1 else idx
	        
	   idx = (prob*geneSize).floor.toInt
	   val gIdx = if(idx == 0) 1 else if(idx == geneSize) geneSize-1 else idx
            
       GeneticIndices(chIdx, gIdx)	
    }
}


		/**
		 * Companion object for the Population class that defines its constructor.
		 */
object Population{
   final val MAX_NUM_CHROMOSOMES = 10000
   def apply[T <: Gene](maxNumChromosomes: Int, chromosomes: Pool[T]): Population[T] = new Population[T](maxNumChromosomes, chromosomes)
   def apply[T <: Gene](maxNumChromosomes: Int, chromosomes: List[Chromosome[T]]): Population[T] = new Population[T](maxNumChromosomes, new Pool[T] ++ chromosomes)
}

// -----------------------  EOF -------------------------------------------