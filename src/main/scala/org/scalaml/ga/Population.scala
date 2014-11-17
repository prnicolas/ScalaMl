/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
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
		 * an operator such as mutation or cross-over acts upon (hierarchical address),>/p>
		 * @constructor Create a genetic indexer. 
		 * @param chOpIdx  Index of the gene in the chromosome, that is manipulated by a genetic operator
		 * @param geneOpIdx Index of the bits in the gene that is manipulated by a genetic operator.
		 * @author Patrick Nicolas
		 * @since June 7, 2014
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm/Genetic algorithm components
		 */
case class GeneticIndices(val chOpIdx: Int, val geneOpIdx: Int)

		/**
		 * <p>Class that defines a population of chromosomes. The size of the population varies
		 * over time following successive, iterative selection but is bounded to avoid a potential
		 * explosion of candidates/chromosomes.<br>
		 * <b>limit</b>  Maximum number of chromosomes allowed in this population (constrained optimization) <br>
		 * <b>chromosomes</b> Current pool of chromosomes
		 * @constructor Create a population of chromosome. [chromosomes] Current pool of chromosomes
		 * @throws IllegalArgumenException if the limit is out of range or the pool of chromosomes is undefined
		 * 
		 * @author Patrick Nicolas
		 * @since August 25, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm/Genetic algorithm components
		 */
final class Population[T <: Gene](limit: Int, val chromosomes: Pool[T]) {
	import Population._
	
	check(limit, chromosomes)
	private val logger = Logger.getLogger("Population")
    	
		/**
		 * <p>Add an array of chromosomes (or new population) to this existing population and return
		 * a new combined population. The new chromosomes are appended to the existing pool</p>
		 * @param that New population to be added to the existing population
		 * @throws IllegalArgumentException if the population is undefined
		 * @return The combined population if the new population is not empty, this population otherwise
		 */
	def + (that: Population[T]): Population[T] = {
		require(that != null, "Population.+: Cannot add an undefined list of chromosomes to this population")
		if(that.size > 0) 
			Population[T](limit, chromosomes ++: that.chromosomes) 
		else this
	}
    

		/**
		 * <p>Add a new Chromosome to this population using a list of genes.</p>
		 * @param newCode Genetic code (List of genes) for the new chromosome added to this population
		 * @throws IllegalArgumentException if the newCode is either undefined or has an incorrect size.
		 */
	protected def += (newCode: List[T]): Unit = {
		require(newCode != null, "Population.+=: Cannot add an undefined chromosome to this population")
		require(newCode.size == chromosomes(0).size, s"Population.+=: The number of genes ${newCode.size} is inconsistent the chromosome size ${chromosomes(0).size}")
		chromosomes += new Chromosome[T](newCode)
	}
  
    	
		/**
		 * <p>Selection operator for the chromosomes pool The selection relies on the
		 * normalized cumulative unfitness for each of the chromosome ranked by decreasing
		 * order.</p>
		 * @param score Scoring function applied to all the chromosomes of this population
		 * @param cutOff Normalized threshold value for the selection of the fittest chromosomes
		 * @throws IllegalArgumenException if the cutoff is out of bounds or the scoring function is undefined
		 */
	def select(score: Chromosome[T]=> Unit, cutOff: Double): Unit = {
		require(score != null, "Population.select Cannot select chromosomes in a population with undefined fitness function")
		require(cutOff > 0.0 && cutOff < 1.0, s"Population.select Cannot select with a cutoff $cutOff out of range")
		
		val cumul = chromosomes.foldLeft(0.0)((s, xy) => {score(xy); s + xy.unfitness })
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
		 * @return number of bits in the gene that compose the chromosomes of this population if the population is not empty, -1 otherwise
		 */
	@inline
	final def geneSize: Int = if(chromosomes.size > 0) chromosomes.head.code.head.size else -1
    
		/**
		 * <p>Return the number of genes in the chromosomes of this population.</p>
		 * @return Number of genes in each of the chromosomes of this population if the population is not empty, -1 otherwise
		 */
	@inline
	final def chromosomeSize: Int = if(chromosomes.size > 0) chromosomes.head.size else -1
    
    
		/**
		 * <p>Applies the cross-over operator on the population by pairing
		 * the half most fit chromosomes with the half least fit chromosomes.</p>
		 * @param xOver cross-over factor [0, 1]
		 * @throws IllegalArgumentException if xOver is out of range.
		 */
	def +- (xOver: Double): Unit = {
		require(xOver > 0.0 && xOver < 1.0, s"Population.+- Cross-over factor $xOver on the population is out of range")
    	  	
		if( size > 1) {
			val mid = size>>1
			val bottom = chromosomes.slice(mid,  size)              
			val gIdx = geneticIndices(xOver)
			val offSprings = chromosomes.take(mid).zip(bottom).map(p => p._1 +- (p._2, gIdx)).unzip
			chromosomes ++= offSprings._1 ++ offSprings._2
		}
	}
    
    
		/**
		 * <p>Apply the mutation of the population. The operation produces a duplicate set of 
		 * chromosomes that are mutated using the mutate operator ^ on chromosome.</p>
		 * @param mu mutation factor
		 * @return Population with original chromosomes and mutated counter-part
		 * @throws IllegalArgumenException if mu is out of range [0, 1]
		 */
	def ^ (mu: Double): Unit = {
		require(mu > 0.0 && mu < 1.0, s"Population.^ Mutation factor $mu on the population is out of range")
		chromosomes ++= chromosomes.map(_ ^ geneticIndices(mu))
	}

	
		/**
		 * <p>Compute the difference between the N fittest chromosomes of two populations.</p>
		 * @param that The population to be compared to
		 * @param depth Number of fittest chromosomes used in the comparison. If the depth exceeds the size the entire population is used in the comparison
		 * @return The depth fittest chromosomes if there are common to both population, None otherwise
		 * @throws IllegalArgumenException if mu is out of range [0, 1]
		 */
	final def diff(that: Population[T], depth: Int): Option[Pool[T]] = {
		require(that != null, "Population.diff Other population is undefined")
		require(depth > 0, s"Population.diff depth $depth should be >1")
		
		val fittestPoolSize = {
			if(depth >= size || depth >= that.size) 
				if(size < that.size) size else that.size
			depth
		}
		
		fittest(fittestPoolSize) match {
			case Some(top) => that.fittest(fittestPoolSize) match {
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

	
		/**
		 * <p>Retrieve the N fittest chromosomes from this population</p>
		 * @param Number of fittest chromosomes to retrieve
		 * @return The depth fittest chromosomes if the population is not empty, None otherwise
		 * @throws IllegalArgumentException If depth is not greater than 0
		 */
	final def fittest(depth: Int) : Option[Pool[T]] = {
		require(depth > 0, s"Population.fittest Cannot list a negative or null number of chromosomes: $depth")
		
		if( size > 1) 
			Some(chromosomes.take(if(depth > size)  size else depth))
		else 
			None
	}
	
	
		/**
		 * <p>Retrieve the number of chromosomes in the population</p>
		 * @return Number of chromosomes in this population
		 */
	@inline
	final def size: Int = chromosomes.size
	

		/**
		 * Textual description of the genetic code of this population
		 * @return Genetic code for all the chromosomes of this population
		 */
	override def toString: String = 
		chromosomes.foldLeft(new StringBuilder)((buf, x) =>  { buf.append(s"${x.toString}\n")}).toString

		/**
		 * Symbolic representation of this population
		 * @return Symbolic representation all the chromosomes of this population
		 */
	final def symbolic(comments: String): Unit = 	
		chromosomes.foldLeft(new StringBuilder(comments))((buf, x) => 
			buf.append(s"${x.symbolic("->")}\n")).toString

	
	private[this] def geneticIndices(prob: Double): GeneticIndices = {
		var idx = (prob*chromosomeSize).floor.toInt
		val chIdx = if(idx == 0) 1 else if(idx == chromosomeSize) chromosomeSize-1 else idx
	        
		idx = (prob*geneSize).floor.toInt
		val gIdx = if(idx == 0) 1 else if(idx == geneSize) geneSize-1 else idx
		GeneticIndices(chIdx, gIdx)	
	}
}


		/**
		 * Companion object for the Population class that defines its constructor
		 * and validate its parameters.
		 */
object Population{
	final val MAX_NUM_CHROMOSOMES = 10000
   
	private def check[T <: Gene](limit: Int, chromosomes: Pool[T]): Unit  = {
		require(chromosomes != null, "Population.check: The population has undefined initial set of chromosomes")
		require(chromosomes.size > 0 && chromosomes.size < limit, s"Population.check: The pool of chromosomes ${chromosomes.size} is out of range")
		require(limit > 1 && limit < MAX_NUM_CHROMOSOMES, s"Maximum number of allowed chromosomes $limit is out of range")
	}

	def apply[T <: Gene](limit: Int, chromosomes: Pool[T]): Population[T] = 
		new Population[T](limit, chromosomes)
		
	def apply[T <: Gene](maxNumChromosomes: Int, chromosomes: List[Chromosome[T]]): Population[T] = 
		new Population[T](maxNumChromosomes, new Pool[T] ++ chromosomes)
}

// -----------------------  EOF -------------------------------------------