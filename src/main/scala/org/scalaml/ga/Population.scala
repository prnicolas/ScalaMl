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
package org.scalaml.ga

import java.util.{HashSet, Arrays}
import scala.collection.mutable.ArrayBuffer

import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl.{DblVector, DblMatrix}
import org.scalaml.core.XTSeries
import org.scalaml.util.DisplayUtils
import Chromosome._
import Population._



		/**
		 * <p>Class that defines relative genetic index (index in the genetic code, 
		 * an operator such as mutation or cross-over acts upon (hierarchical address)</p>
		 * @constructor Create a genetic indexer. 
		 * @param chOpIdx  Index of the gene in the chromosome, that is manipulated by a genetic operator
		 * @param geneOpIdx Index of the bits in the gene that is manipulated by a genetic operator.
		 * @author Patrick Nicolas
		 * @since June 7, 2014
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm / Genetic algorithm components
		 */
case class GeneticIndices(val chOpIdx: Int, val geneOpIdx: Int)

		/**
		 * <p>Class that defines a population of chromosomes. A population is initialized and evolves
		 * through multiple reproduction cycles. The size of the population varies
		 * over time following successive, iterative selection but is bounded by an upper limit to 
		 * avoid a potential explosion of the number chromosomes.</p>
		 * @constructor Create a population of chromosome. [chromosomes] Current pool of chromosomes
		 * @throws IllegalArgumenException if the limit is out of range or the pool of chromosomes 
		 * is undefined
		 * @param limit Maximum number of chromosomes allowed in this population 
		 * (constrained optimization)
		 * @param chromosomes Current pool of chromosomes (type: ArrayBuffer{Chromosome[T]])
		 * @author Patrick Nicolas
		 * @since August 25, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm / Genetic algorithm components
		 */
class Population[T <: Gene](limit: Int, val chromosomes: Pool[T]) {
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
		require( !that.isNull, "Population.+: Cannot add an undefined list of chromosomes")
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
		require( !newCode.isEmpty, "Population.+=: Cannot add an undefined chromosome")
		require(newCode.size == chromosomes(0).size, 
				s"Population.+=: Number of genes ${newCode.size} != chromosome size ${chromosomes(0).size}")
		chromosomes += new Chromosome[T](newCode)
	}
  
		/**
		 * <p>Selection operator for the chromosomes pool The selection relies on the
		 * normalized cumulative unfitness for each of the chromosome ranked by decreasing
		 * order.</p>
		 * @param score Scoring function applied to all the chromosomes of this population
		 * @param cutOff Normalized threshold value for the selection of the fittest chromosomes
		 * @throws IllegalArgumenException if the cutoff is out of bounds
		 */
	def select(score: Chromosome[T]=> Unit, cutOff: Double): Unit = {
		require(cutOff > 0.0 && cutOff < 1.01, 
				s"Population.select Cannot select with a cutoff $cutOff out of range")
				
			// Compute the cumulative score for the entire population
		val cumul = chromosomes.foldLeft(0.0)((s, xy) => {
			score(xy)
			s + xy.unfitness 
		})
			// Normalize each chromosome unfitness value
		chromosomes foreach( _ /= cumul)
		
			// Sorts the chromosome by the increasing value of their unfitness
		val newChromosomes = chromosomes.sortWith(_.unfitness < _.unfitness)

			// Apply a cutoff value to the current size of the population
			// if the cutoff has been defined.
		val cutOffSize: Int = (cutOff*newChromosomes.size).floor.toInt
		val newPopSize = if(limit < cutOffSize) limit else cutOffSize
		chromosomes.clear
		chromosomes ++= newChromosomes.take(newPopSize)        
	}
    

		/**
		 * <p>Return the size of the genes that compose the chromosomes of this population. 
		 * It is assumed that the genes in the chromosomes have identical size.</p>
		 * @return number of bits in the gene that compose the chromosomes of this population if 
		 * the population is not empty, -1 otherwise
		 */
	final def geneSize: Int = if(chromosomes.size > 0) chromosomes.head.code.head.size else -1
    
		/**
		 * <p>Return the number of genes in the chromosomes of this population.</p>
		 * @return Number of genes in each of the chromosomes of this population if the population 
		 * is not empty, -1 otherwise
		 */
	final def chromosomeSize: Int = if(chromosomes.size > 0) chromosomes.head.size else -1
    
		/**
		 * <p>Applies the cross-over operator on the population by pairing
		 * the half most fit chromosomes with the half least fit chromosomes.</p>
		 * @param xOver cross-over factor [0, 1]
		 * @throws IllegalArgumentException if xOver is out of range.
		 */
	def +- (xOver: Double): Unit = {
		require(xOver > 0.0 && xOver < 1.0, 
				s"Population.+- Cross-over factor $xOver on the population is out of range")
  
				// It makes sense to cross over all the chromosomes in this
				// population if there are more than one chromosome
		if( size > 1) {
				// Breakdown the sorted list of chromosomes into two segments
			val mid = size>>1
			val bottom = chromosomes.slice(mid,  size)
			
				// Pair a chromosome for one segment with a chromosome
				// from the other segment.Then add those offsprings to the
				// current population
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
		 * @throws IllegalArgumenException if the mutation ratio or coef. mu is out of range [0, 1]
		 */
	def ^ (mu: Double): Unit = {
		require(mu > 0.0 && mu < 1.0, 
				s"Population.^ Mutation factor $mu on the population is out of range")
		chromosomes ++= chromosomes.map(_ ^ geneticIndices(mu))
	}

	
		/**
		 * <p>Compute the difference between the N fittest chromosomes of two populations.</p>
		 * @param that The population to be compared to
		 * @param depth Number of fittest chromosomes used in the comparison. If the depth exceeds 
		 * the size the entire population is used in the comparison
		 * @return The depth fittest chromosomes if there are common to both population, None otherwise
		 * @throws IllegalArgumenException if mu is out of range [0, 1]
		 */
	final def diff(that: Population[T], depth: Int): Option[Pool[T]] = {
		require( that.size > 1 , "Population.diff Other population has no chromosome")
		require(depth > 0, s"Population.diff depth $depth should be >1")

			// Define the number of chromosomes participating
			// to the comparison of two populations 'this' and 'that'
		val fittestPoolSize = {
			if(depth >= size || depth >= that.size) 
				if(size < that.size) size else that.size
			depth
		}
			// Deals with nested options. Get the 'depth' most fit
			// chromosomes for this population and 'depth' most fit
			// chromosomes for that population, then compare..
		for {
			first <- fittest(fittestPoolSize)
			second <- that.fittest(fittestPoolSize)
			if( !first.zip(second).exists( x => x._1 != x._2 ) )
		} yield first
	}

	
		/**
		 * <p>Retrieve the N fittest chromosomes from this population</p>
		 * @param Number of fittest chromosomes to retrieve
		 * @return The depth fittest chromosomes if the population is not empty, None otherwise
		 * @throws IllegalArgumentException If depth is not greater than 0
		 */
	final def fittest(depth: Int) : Option[Pool[T]] = {
		require(depth > 0, 
				s"Population.fittest Incorrect number of chromosomes: $depth should be >0")
		if( size > 1) Some(chromosomes.take(if(depth > size)  size else depth)) else None
	}
	
	
		/**
		 * <p>Retrieve the number of chromosomes in the population</p>
		 * @return Number of chromosomes in this population
		 */
	final def size: Int = chromosomes.size
	
	final def isNull: Boolean = chromosomes.isEmpty

		/**
		 * Textual description of the genetic code of this population
		 * @return Genetic code for all the chromosomes of this population
		 */
	override def toString: String = chromosomes.map(_.toString).mkString("\n")

		/**
		 * Symbolic representation of this population
		 * @return Symbolic representation all the chromosomes of this population
		 */
	final def symbolic(comments: String): Unit = 	chromosomes.map(_.symbolic("->")).mkString("\n")

			/*
			 * Compute the genetic index for cross-over and mutation
			 * according to a probability value
			 * @param prob probability value [0, 1]
			 */
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
		 * @author Patrick Nicolas
		 * @since August 25, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm/Genetic algorithm components
		 */
object Population{
		/**
		 * Default constructor for the population of chromosomes
		 * @param limit  Maximum number of chromosomes allowed in this population (constrained optimization)
		 * @param chromosomes Current pool of chromosomes (type: ArrayBuffer{Chromosome[T]])
		 */
	def apply[T <: Gene](limit: Int, chromosomes: Pool[T]): Population[T] = 
			new Population[T](limit, chromosomes)

		/**
		 * Default constructor for the population of chromosomes
		 * @param limit  Maximum number of chromosomes allowed in this population (constrained 
		 * optimization)
		 * @param chromosomes New list of chromosomes added to the existing pool
		 */
	def apply[T <: Gene](limit: Int, chromosomes: List[Chromosome[T]]): Population[T] = 
			new Population[T](limit, new Pool[T] ++ chromosomes)

	private val MAX_NUM_CHROMOSOMES = 10000
	
	private def check[T <: Gene](limit: Int, chromosomes: Pool[T]): Unit  = {
		require( !chromosomes.isEmpty, 
				"Population.check: Undefined initial set of chromosomes")
		require(chromosomes.size > 0 && chromosomes.size < limit, 
				s"Population.check: The pool of chromosomes ${chromosomes.size} is out of range")
		require(limit > 1 && limit < MAX_NUM_CHROMOSOMES, 
				s"Maximum number of allowed chromosomes $limit is out of range")
	}
	
		/**
		 * Define a Null population for error checking purpose
		 */
	def nullPopulation[T <:Gene]: Population[T] = new Population[T](-1, ArrayBuffer.empty)
}

// -----------------------  EOF -------------------------------------------