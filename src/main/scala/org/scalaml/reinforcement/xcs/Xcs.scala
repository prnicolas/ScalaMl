/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
 */
package org.scalaml.reinforcement.xcs

import org.scalaml.ga._
import org.scalaml.trading.Signal
import org.scalaml.reinforcement.qlearning._
import org.scalaml.core.types.ScalaMl._
import Chromosome._
import org.scalaml.core.design.PipeOperator



		/**
		 * <p>Example of implementation of the XCS algorithm with a predefined
		 * configuration and a set of training episode for the Q-Learning algorithm used to assign
		 * credit to individual rules that improve the performance (or objective
		 * function) of a system.<br>
		 * This implementation assumes that the new data (Signal) and reward following the
		 * previous set of actions on the system are collected at the same time.</p>
		 * @constructor Create an extended learning classifiers. [config] Configuration the XCS algorithm (GA and Q-Learning parameters). [input] Training episodes used to create the Q-Learning model. [score] Chromosome scoring function. [population] Initial population of classifiers or rule.
		 * @param config for the XCS algorithm (GA and Q-Learning parameters)
		 * @param qLabels training episodes used to create the Q-Learning model
		 * @throws IllegalArgumenException if the configuration, input information or training episodes is undefined
		 * 
		 * @author Patrick Nicolas
		 * @data March 26, 2014
		 * @note Scala for Machine Learning Chapter 11 Reinforcement learning/Learning classifier systems.
		 */

case class XcsSensor(val id: String, val value: Double)

final class Xcs(config: XcsConfig, population: Population[Signal], score: Chromosome[Signal]=> Unit, input: Array[QLInput]) extends PipeOperator[XcsSensor, List[XcsAction]] {

   require(config != null, "Xcs: Cannot create XCS with undefined configuration")
   require(score != null, "Xcs: Cannot create XCS with undefined chromosome scoring function")
   require(input != null, "Xcs: Cannot create XCS with undefined state input")
   require(population != null, "Xcs: Cannot create XCS with undefined population")
   require(population.size > 2, s"Xcs: Cannot create XCS with a population of size ${population.size}")
   	      
   val gaSolver = GASolver[Signal](config.gaConfig, score)   
   val featuresSet: Set[Chromosome[Signal]]  = population.chromosomes.toSet
   val qLearner = QLearning[Chromosome[Signal]](config.qlConfig, computeNumStates(input), extractGoals(input), input, featuresSet)
   
   private def extractGoals(input: Array[QLInput]): Int = -1
   private def computeNumStates(input: Array[QLInput]): Int = -1
   
   override def |> : PartialFunction[XcsSensor, List[XcsAction]] = {
     case _ => List.empty
   }
}


          
  
// ------------------------------------  EOF -----------------------------------------------------