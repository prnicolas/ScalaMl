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
package org.scalaml.reinforcement.xcs

import org.scalaml.ga._
import org.scalaml.trading.Signal
import org.scalaml.reinforcement.qlearning._
import org.scalaml.core.Types.ScalaMl._
import Chromosome._
import XcsRule._
import Xcs._
import org.scalaml.workflow.PipeOperator



		/**
		 * <p>Example of implementation of the XCS algorithm with a predefined
		 * configuration and a set of training episode for the Q-Learning algorithm used to assign
		 * credit to individual rules that improve the performance (or objective
		 * function) of a system.<br>
		 * This implementation assumes that the new data (Signal) and reward following the
		 * previous set of actions on the system are collected at the same time.</p>
		 * @param config configuration for the XCS algorithm (GA and Q-Learning parameters)
		 * @param qLabels training episodes used to create the Q-Learning model
		 * @throws IllegalArgumenException if the configuration, input information or training episodes is undefined
		 * 
		 * @author Patrick Nicolas
		 * @data March 26, 2014
		 * @note Scala for Machine Learning
		 */
import Xcs._
final class Xcs(val config: XcsConfig) extends PipeOperator[XcsInput, List[XcsAction[Double]]] {

   require(config != null, "Cannot create XCS with undefined configuration")
   	
   val gaSolver = GASolver[XcsRule[Double]](config.gaConfig, config.init)
   var history = List[(DblVector, XcsCredit)](); 
   
        /**
         * <p>Method that updates the existing population of rules or policies
         * following an input from the system and generates a list of XcsAction to be 
         * performed on a system.</p>
         * @param xcsInput (signal, reward) data
         * @return list of actions to be performed if the credit can be computed and 
         * distributed across the fittest chromosomes and the population can be updated, 
         * None otherwise. 
         * @throws IllegalArgumentException if xcsInput is undefined.
         */
   override def |> (xcsInput: XcsInput): Option[List[XcsAction[Double]]] = {
  	  require(xcsInput != null,  "Cannot extract next set of XCS action for undefined signal or reward input" )
  	 
  	  	// Attempt to match any chromosome from the population
  	    // that has a rule/gene which predicate/signal match the new signal
  	  val matchingChromosomes = matches(xcsInput._1)
  	  
  	    // No match so need to cover (generate more chromosomes)
  	  if (matchingChromosomes.isEmpty ) {
  	  	 val rulesList = XcsCover.create(xcsInput._1, gaSolver.populationSize)
  	  	 gaSolver.population += rulesList
  	  }
  	  
  	  	// we have a set of matching rules..
  	  else 
  	  	assignCredit(xcsInput._2, matchingChromosomes)
  	  
  	    // We apply the genetic selection to the new population
  	   selectActions
   }
  	  
  	  
  	  
  	private def assignCredit(reward: XcsReward, matchingChromosomes: List[Chromosome[XcsRule[Double]]]): Unit = {
  		  	  		// intializes the Q-Learning algorithm
  	    val qLearning = QLearning[Double](config.qConfig, QLLabel[Double](history), XcsRule.rulesCount)
  	        // Compute the credit to be assigned to the matching chromosomes.
  	    val newCredit = XcsCredit( matchingChromosomes.map(_.fitness ) )
  	  	
  
  	    qLearning |>  (reward, newCredit) match {
  	  	   case Some(newState) => {
  	  	      if(newState._1.distribution.size == matchingChromosomes.size)
  	  	         matchingChromosomes.zipWithIndex.foreach( x => x._1.fitness = newState._1.distribution(x._2))
  	  	      else
  	  	      	println("The size of the distribution of the credit " + newState._1.distribution.size + " does not match the number of matching chromosomes")
  	  	   }
  	  	   case None => println("Failed computing and assigning credit to deserving chromosomes")
  	    }
  	    	// update the history for the Q-Learning algorithm
  	     history = (reward, newCredit) :: history
   }	 
   
   private def selectActions: Option[List[XcsAction[Double]]] = {
  	   if( gaSolver.search(ruleFitness) == GAState.SUCCEED) {
  	      gaSolver.population.topChromosomes(1) match {
  	  	     case Some(bestPolicy) => Some(bestPolicy(0).code.map( _.action))
  	  		 case None => println("Xcs failed to extract the best rule"); None
  	  	  }
  	   }
  	   else {
  	  	  println("Xcs failed to select rules"); None
  	   }
   }
   

   private def matches(signal: Signal): List[Chromosome[XcsRule[Double]]] = {
  	  gaSolver.population.chromosomes.filter( chr => {
  	  	 chr.code.find( _.signal ==signal) match {
  	  		 case Some(c) => true
  	  		 case None => false
  	  	  }
  	  }).toList
   }
}


object Xcs {
	type XcsReward = DblVector
	type XcsInput = (Signal, XcsReward)
	
	def ruleFitness(policy: Chromosome[XcsRule[Double]]): Double = {
	  require( policy != null,  "Cannot commpute fitness of undefined policy")
	  
	  -1.0
	}
}


          
  
// ------------------------------------  EOF -----------------------------------------------------