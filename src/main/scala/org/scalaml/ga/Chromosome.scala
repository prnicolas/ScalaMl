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

import scala.util.Random
import scala.annotation.implicitNotFound
import scala.collection._

import org.scalaml.core.Types.emptyString
import Gene._, Chromosome._


		/**
		 * Class that implements a parameterized chromosome using an encoding scheme and
		 * an objective/fitness function. A chromosome is a container or list of Gene that
		 * represents candidate solution to a problem or candidate model to a dataset.
		 * @tparam T Paramertized type upper bounded by '''Gene'''
		 * @constructor Create a chromosome with the parameterized sub-type of Gene
		 * @throws IllegalArgumentException if the genetic code is undefined or empty
		 * @param code List of Genes or sub types composing this chromosomes.
		 * 
		 * @author Patrick Nicolas
		 * @since August 27, 2013 (0.97)
		 * @version 0.98.2
		 * @see Scala for Machine Learning Chapter 10 Genetic Algorithm / Genetic algorithm components
		 * @note This particular implementation computes the chromosome cost or unfitness. 
		 * The fitness value of a chromosome is computes as 1/cost
		 */
@throws(classOf[IllegalArgumentException])
final class Chromosome[T <: Gene](val code: List[T]) {  
	require( code.nonEmpty, "Chromosome Cannot create a chromosome from undefined genes")
	var cost: Double = COST_FACTOR*(1.0 + Random.nextDouble)

	
		/**
		 * Define the cross-over operator to be applied on this chromosome. The cross-over
		 * is hierarchical. The algorithm selects the gene associated to the cross-over index, 
		 * swap all the genes of higher index (below or after the cross-over gene) between
		 * the two parents and finally swap the bits within the cross-over gene.
		 * 
		 * 
		 * The cross over operation generates two off springs from the two original parents. 
		 * The off-springs are added to the current population along with the parents.
		 * @param that other parent chromosome
		 * @param indices Genetic index for the cross-over.
		 * @throws IllegalArgumentException if the other chromosome is undefined, or have a 
		 * different size  or if the cross-over factor is out of range.
		 * @return the pair of offspring chromosomes
		 */
	@throws(classOf[IllegalArgumentException])
	def +- (that: Chromosome[T], indices: GeneticIndices): (Chromosome[T], Chromosome[T]) = {
		require(!that.isEmpty, 
				"Chromosome.+-  Cannot cross-over chromosome with an undefined parent")
		require(this.size == that.size, 
				s"Chromosome.+- Chromosomes $size and that ${that.size} have different size")

			// First use the global index (module the number of gene
		val xoverIdx = indices.chOpIdx
		val xGenes =  spliceGene(indices, that.code(xoverIdx) ) 
		
			// Then recombine the offspring along the cross-over bit
		val offSprng1 = code.slice(0, xoverIdx) ::: xGenes._1 :: that.code.drop(xoverIdx+1)
		val offSprng2 = that.code.slice(0, xoverIdx) ::: xGenes._2 :: code.drop(xoverIdx+1)
		
			// Returns the two offsprings that are added to the current population
		(Chromosome[T](offSprng1), Chromosome[T](offSprng2))
	}
   
   		
		/**
		 * Mutation operator that flip a gene selected through a mutation index.
		 * The mutated gene is added to the population (gene pool).
		 * @param indices Genetic index
		 * @throws IllegalArgumentException if mutation coefficient, mu is out of range
		 * @return A new mutated chromosome
		 */
	def ^ (indices: GeneticIndices): Chromosome[T] = {
			// Get the mutation index in the gene to mutate, chOpIdx
		val mutated = code(indices.chOpIdx) ^ indices
		
			// Flip the bit at index 'gIdx.chOpIdx,
		val xs = code.indices.map(i =>
			if(i == indices.chOpIdx) mutated.asInstanceOf[T] else code(i)
		).toList
		Chromosome[T](xs)
	}
     
     
		/**
		 * Normalize the fitness of this chromosome with a factor. This 
		 * operation is required by the selection algorithm.
		 * @param normalizeFactor normalization factor
		 * @throws IllegalArgumentException if the normalization factor is less than EPS
		 */
	@throws(classOf[IllegalArgumentException])
	def /= (normalizeFactor: Double): Unit = {
		require( Math.abs(normalizeFactor) > Chromosome.EPS, 
				s"Chromosome./= Cannot normalize with $normalizeFactor > ${Chromosome.EPS}")
		cost /= normalizeFactor
	}

		/**
		 * Decode this chromosome by applying a type conversion from Gene to T
		 * @param d implicit conversion of Gene to the parameterized type T which is a sub-class of Gene
		 * @throws ImplicitNotFoundException if the implicit conversion d is undefined.
		 */
	@implicitNotFound(msg = "Chromosome.decode Conversion from Gene to $T is undefined")
	def decode(implicit d: Gene => T): List[T] = code.map( d(_)) 

		/**
		 * Clone this chromosome by duplicating its genes
		 * @return Duplicate of this chromosome
		 */
	override def clone: Chromosome[T] = Chromosome[T](code)
    
		/**
		 * Returns the size of this chromosome as the number of genes it contains,.
		 * @return Number of genes
		 */
	@inline
	final def size: Int = code.size
	
	final def isEmpty: Boolean = code.isEmpty
	
	final def fitness: Double = if( cost < EPS) 1e+10 else 1.0/cost
	
		/**
		 * Stringize the genetic code of this chromosome
		 * @return Genetic code {0, 1} for this chromosome
		 */
	override def toString: String = String.valueOf(code.toString())
     
	
		/**
		 * Symbolic representation of the chromosome as a sequence of symbolic representation
		 * of the genes it contains
		 * @return sequence of symbolic representation of the genes of this chromosomes
		 */
	final def symbolic: String = 
		s"${code.map( _.symbolic).mkString(" ")} cost= $cost fitness: $fitness"

			/*
			 * Auxiliary method to splice this chromosome with another 
			 * chromosome with the genetic material thatCode, along the
			 * bit of index gIdx
			 */
	private def spliceGene(indices: GeneticIndices, thatCode: T): (T, T) = {
		((this.code(indices.chOpIdx) +- (thatCode, indices)).asInstanceOf[T], 
		(thatCode +- (code(indices.chOpIdx), indices)).asInstanceOf[T] )
	}
}


		/**
		 * Companion object to a Chromosome used to define the constructors
		 * @author Patrick Nicolas
		 * @since September 2, 2013 v 0.97
		 * @version 0.98.2
		 * @see Scala for Machine Learning Chapter 10 Genetic Algorithm/Genetic algorithm components
		 */
object Chromosome {
	import java.util.BitSet
  
	private val EPS = 1e-10
	val COST_FACTOR = 500.0

		/**
		 * Default (Generic code) constructor for the Chromosome
		 * @param code List of Genes or sub types composing this chromosomes.
		 */
	def apply[T <: Gene](code: List[T]): Chromosome[T] = new Chromosome[T](code)
	
		/**
		 * Symbolic constructor for the Chromosome
		 * @param predicates List of predicates of type T for this chromosome
		 * @param encode Function that convert a predicate to a Gene
		 * @throws IllegalArgumentException if either the predicates are undefined (empty)
		 * @return Chromosome built from predicates and an encoding function
		 */
	@throws(classOf[IllegalArgumentException])
	def apply[T <: Gene](predicates: List[T], encode: T => Gene): Chromosome[T] = {
		require( predicates.nonEmpty,
				"Chromosome.apply List of predicates is undefined")

			// Create a chromosome with a single gene for a single predicate 
			// or a list of gene for multiple predicate.
			// A gene is actually generated by encoding the predicate
		new Chromosome[T](if(predicates.size == 1) 
			List[T](encode(predicates.head).asInstanceOf[T])
		else 
			predicates./:(List[T]()) ((xs, t) => encode(t).asInstanceOf[T] :: xs))
	}
			
		/**
		 * Type for the pool of chromosomes. A Pool of chromosomes is an arbitrary
		 * array of chromosomes.
		 */
	type Pool[T <: Gene] = mutable.ArrayBuffer[Chromosome[T]]
	
			/**
			 * Define a Null Chromosome
			 */
	def nullChromosome[T <: Gene]: Chromosome[T] = new Chromosome[T](List.empty[T])
}

// ------------------------  EOF --------------------------------------