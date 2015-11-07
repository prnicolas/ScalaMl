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
 * Version 0.99
 */
package org.scalaml.reinforcement.xcs

import scala.util.Try

import org.scalaml.ga._
import org.scalaml.trading.Signal
import org.scalaml.reinforcement.qlearning._
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.ETransform



		/**
		 * Class that defines a sensor (or input stimuli) in an extended learning classifier system. 
		 * It is assumed that the XCS model monitors continuous values of type Double
		 * @param id Identifier for the sensor or stimuli
		 * @param value value of the stimuli or sensor.
		 * 		 
		 * @author Patrick Nicolas
		 * @since March 26, 2014
		 * @note Scala for Machine Learning Chapter 11 Reinforcement learning / Extended learning 
		 * classifier systems
		 */
case class XcsSensor(val id: String, val value: Double)

		/**
		 * Example of implementation of the XCS algorithm with a predefined
		 * configuration and a set of training episode for the Q-Learning algorithm used to assign
		 * credit to individual rules that improve the performance (or objective
		 * function) of a system. 
		 * @constructor Create an extended learning classifiers system.
		 * @throws IllegalArgumenException if the configuration, input information or training 
		 * episodes is undefined
		 * @param config  Configuration for the XCS algorithm (GA and Q-Learning parameters)
		 * @param population Initial population for the search space of classifiers
		 * @param score	Chromosome scoring function
		 * @param input Input for Q-learning state transition space QLSpace used in training
		 * @author Patrick Nicolas
		 * @since March 26, 2014
		 * @note Scala for Machine Learning Chapter 11 Reinforcement learning / Extended learning 
		 * classifier systems
		 */
final class Xcs(
		config: XcsConfig, 
		population: Population[Signal], 
		score: Chromosome[Signal]=> Unit, 
		input: Array[QLInput]) extends ETransform[XcsConfig](config) {
  
	import Xcs._
  
  type U = XcsSensor
  type V = List[XcsAction]
	check(population, input)

	val gaSolver = GASolver[Signal](config.gaConfig, score)   
	val features: Seq[Chromosome[Signal]]  = population.chromosomes.toSeq
	val qLearner = QLearning[Chromosome[Signal]](config.qlConfig, extractGoals(input), input, features)
   
	private def extractGoals(input: Array[QLInput]): Int = -1
	private def computeNumStates(input: Array[QLInput]): Int = -1
  
	override def |> : PartialFunction[U, Try[V]] = {
		case _ => Try(List.empty[XcsAction])
	}
}	


		/**
		 * Companion object for the extended learning classifier system.
		 */
object Xcs {
	protected def check(
			population: Population[Signal], 
			input: Array[QLInput]): Unit = {
	  
		require( !input.isEmpty, "Xcs.check: Cannot create XCS with undefined state input")
		require( !population.isEmpty, "Xcs.check: Cannot create XCS with undefined population")
		require(population.size > 2, 
				s"Xcs.check: Cannot create XCS with a population of size ${population.size}")
	}
}


// ------------------------------------  EOF -----------------------------------------------------