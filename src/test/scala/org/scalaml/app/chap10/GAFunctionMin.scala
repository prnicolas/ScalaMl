/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used 
 * to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98.1
 */
package org.scalaml.app.chap10

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

import org.scalaml.ga._
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.util.{FormatUtils, DisplayUtils}

import org.scalaml.app.Eval
import Chromosome._


		/**
		 * <p>Class that models a simple gradient of a function. This simplistic representation of
		 * the gradient is defined a gene with the delta_x value as target and FDirector of change 
		 * as operator (type FDirection). The initial value x0 is also a class parameter.<br>
		 * </b>f(x0 + delta) = f(x0) + gradient*delta</b><br>
		 * gradient = [f(x0 + delta) - f(x0)]/delta</p>
		 * @param id Identifier of this gradient
		 * @param delta delta(x) = x - x0 used in the gradient denominator
		 * @param op Direction of the change as incr for delta > 0 and decr for delta < 0
		 * @param x0 Initial value of the function.
		 */
final protected class FGradient (
		id: String, 
		delta: Double, 
		op: FDirection,
		x0: Double)(implicit discr: Discretization) extends Gene(id, delta, op) {
		
  		/**
		 * <p>Virtual constructor used in cloning, mutation and cross-over of gene, that
		 * generate an instance of appropriate type.</p>
		 * @param id identifier for the gradient
		 * @param delta delta value used in the gradient
		 * @param op simple operator that increase or decrease the value x for the operator
		 * @return a new instance of the gradient with delta and direction modified through 
		 * genetic reproduction. The new gradient has the same original value x0 as its parent.
		 */
	override def getGene(id: String, delta: Double, op: Operator): Gene = 
			new FGradient(id, delta, op.asInstanceOf[FDirection], x0)

		/**
		 * Action of the gradient f(x + delta) = f(x) + gradient
		 * @return New value of the function
		 */
	override def score: Double = op(x0, delta)
  
		/**
		 * Symbolic representation of the gradient
		 */
	override def symbolic: String = s"$id: ${op.toString}$delta"
}

	/**
	 * </p>Class that defines the direction of a change (delta) to compute the gradient of a 
	 * function.</p>
	 * @param id Identifier for the directional operator
	 * @param f Function that compute the value f(x0 + delta)
	 */
protected class FDirection(_type: Int, val f: (Double, Double) => Double) extends Operator {
		/**
		 * Type of the directional operator (0 for delta > 0, 1 for delta < 0)
		 */
	override def id: Int = _type
		/**
		 * Return the actual type of the operator, FIncrease or FDecrease
		 * @param type of the directional operator
		 * @return instance of the directional operator (singleton)
		 */
	override def apply(_type: Int): FDirection = if(_type == 0) FIncrease else FDecrease
	

	def apply(x: Double, incr: Double): Double = f(x, incr)
	override def toString: String = s"Error for ${_type}"
}
	
		/**
		 * Singleton that defines the increase operator
		 */
protected object FIncrease extends FDirection(0, (x: Double, incr: Double) => x + incr) {
	override def toString: String = "incr "
}
	
		/**
		 * Singleton that defines the decrease operator
		 */
protected object FDecrease extends FDirection(1, (x: Double, incr: Double) => x - incr) {
	override def toString: String = "decr "
}

		/**
		 * Evaluation test for finding the minimum value of a function y = f(x) using a 
		 * sequence of gradient formula as a chromosome in the genetic algorithm based optimizer
		 * 
		 * @see org.scalaml.ga.{Gene, Chromosome} 
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
object GAFunctionMin extends Eval {
		/**
		 * Name of the evaluation 
		 */
	val name: String = "GAEval"
	
	type ChAction = Chromosome[FGradient]
	
	private val XOVER = 0.9					// Probability or ratio for cross-over
	private val MU = 0.87					// Probability or ratio for mutation
	private val MAX_CYCLES = 200		// Maximum number of iterations during the optimization
	private val softLimit = (n: Int) => 0.95
	private val SEED_SIZE = 10
	private val SEED_SIZE_2 = (SEED_SIZE<<1)
	
			// Minimum values is (x = 4.0, y = 2.5)
	val gf1 = (x: Double) => { val y = x- 4.0; y*y + 2.5 }
	
			// Chromosome scoring function. The scoring function adds the delta from all the
			// Gradients (genes), then apply the function to the delta as
			// f(x) = f(x0) + gradient*(delta1 + delta2 + ..)
	private val scoring = (chr: Chromosome[FGradient]) => {
		val fActions: List[Gene] = chr.code
			// summation of all the delta values 
		val sumDelta =  fActions.map(_.score).reduce( _ + _)
		chr.unfitness = gf1(sumDelta)
	}
		/*
		 * Discretize the 32-bit value into R = 1024 levels
		 */
	private val R = 1024
	implicit private val digitize = new Discretization(R)

	
		/** <p>Execution of the scalatest for <b>GASolver</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Apply Genetic Algorithm to function minimization", logger)
		val x0 = 8
		val MAX_POPULATION_SIZE = 120
		
			// Initialize the encoding of the gradient as a 32 delta value and a single bit directional
			// operator 0 for increase, 1 for decrease
		Gene.geneBits = new Gene.GeneBits(32, 1)

			// Initialize the population and the maximum number of chromosomes or 
			// solution candidates
		val population = Population[FGradient](MAX_POPULATION_SIZE, createInitialPop(x0))
		val config = GAConfig(XOVER, MU, MAX_CYCLES, softLimit)
		
			// Define a monitoring callback to trace each reproduction cycle
		val monitor: Option[Population[FGradient] => Unit] = Some(
			(current: Population[FGradient]) => {
				val topChromosomes = current.fittest(5).map( _.toArray).getOrElse(Array.empty)
				if( !topChromosomes.isEmpty ) {
					topChromosomes.foreach(ch => DisplayUtils.show(ch.symbolic(""), logger))
					DisplayUtils.show( s"GASolver average fitness: ${current.averageScore}", logger)
				}
			}
		)
		
			// Instantiate and execute the genetic solver
		val gaSolver = GASolver[FGradient](config, scoring, monitor)
		val best = gaSolver |> population
			
			// Retrieves the best solution.
		val fittest = best.fittest.get
		scoring(fittest)
		DisplayUtils.show(s"$name Solution ${fittest.symbolic("")}", logger)
	}
	
				/*
			 * Create the initial set or population of FAction
			 */
	private def createInitialPop(x0: Double): Pool[FGradient] = {
		val fActionList = List.tabulate(SEED_SIZE_2)(n => 
				new FGradient(n.toString, 
										(SEED_SIZE_2<<2)*Random.nextDouble, 
										if(Random.nextInt(2)==0x01) FIncrease else FDecrease, 
										x0) )
		
		val fActionList_1: List[FGradient] = fActionList.take(SEED_SIZE)
		val fActionList_2: List[FGradient] = fActionList.takeRight(SEED_SIZE)

		Range(0, 30).foldLeft(new ArrayBuffer[ChAction])((buf, n) => {
			val xs = List[FGradient](
				fActionList_1(Random.nextInt(SEED_SIZE)), 
				fActionList_2(Random.nextInt(SEED_SIZE))
			)
			buf.append(new Chromosome[FGradient](xs))
			buf
		})
	}
}

// ----------------------------  EOF ---------------------------------------