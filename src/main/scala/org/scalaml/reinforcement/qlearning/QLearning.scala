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
package org.scalaml.reinforcement.qlearning

import org.scalaml.util.Matrix
import org.scalaml.core.types.ScalaMl._
import org.scalaml.core.design.{Config, PipeOperator}
import org.scalaml.util.Display

import org.apache.log4j.Logger

import scala.collection.mutable.ArrayBuffer
import scala.util.Random



class QLModel[T](val bestPolicy: QLPolicy[T], val accuracy: Double) {
   override def toString: String = s"Best policy: ${bestPolicy.toString} with accuracy: $accuracy" 
}

		/**
		 * <p>Generic parameterized class that implements the QLearning algorithm. The Q-Learning
		 * model is initialized and trained during the instantiation of the class so it can be 
		 * in the correct state for the run-time prediction. Therefore the class instances have only
		 * two states successfully trained and failed training.<br>
		 * The implementation does not assume that every episode (or training cycle) is successful. 
		 * At completion of the training, the ratio of labels over initial training set is computed.
		 * The client code is responsible to evaluate the quality of the model by testing the ratio
		 * gamma a threshold.</p>
		 * @constructor Create a Q-learning algorithm. [config] Configuration for Q-Learning algorithm. [qlSpace] Initial search space. [qlPolicy] Initial set of policies.
		 * @throws IllegalArgumentException if the configuration or labels are undefined
		 * 
		 * @author Patrick Nicolas
		 * @since January 22, 2014
		 * @note Scala for Machine Learning Chap 11 Reinforcement learning/Q-learning
		 */
final class QLearning[T](config: QLConfig, qlSpace: QLSpace[T], qlPolicy: QLPolicy[T]) 
                                   extends PipeOperator[QLState[T], QLState[T]]  {
   
   import QLearning._
   private val logger = Logger.getLogger("QLearning")
   check(config, qlSpace, qlPolicy)
   
   	/**
   	 * Model parameters for Q-learning.
   	 */
   val model: Option[QLModel[T]] = {
  	   val r = new Random(System.currentTimeMillis)
  	   val successes = Range(0, config.numEpisodes).foldLeft(0)((s, n) => {
  	       val success = train(r)
  	       Display.show(s"\nEpisode # $n success: $success", logger)
  	       s + (if(success) 1 else 0)
  	   })
  	   val accuracy = successes.toDouble/config.numEpisodes
  	   
  	   if( config.minAccuracy == -1.0 || accuracy >= config.minAccuracy )
  	      Some(new QLModel[T](qlPolicy, accuracy))
  	   else None
   }

        /**
   		 * <p>Prediction method using Q-Learning model. The model is automatically 
   		 * trained during instantiation of the class. It overrides the
   		 * usual data transformation method (PipeOperator)</p>
   		 * @throws MatchError if the model is undefined or the input state is undefined 
     * @return PartialFunction of features vector of type Array[T] as input and the predicted vector values as output
   		 * 
   		 * @param data One or several entry as tuple (observations, state)
   		 * @return A tuple (new state, reward) if the training was originally successful and all 
   		 * the rewards associated to the action from the current state can be computed, None otherwise
   		 * @throws IllegalArgumenException if the entry is undefined.
   		 */
      
   override def |> : PartialFunction[QLState[T], QLState[T]] = {
     case state: QLState[T] if(state != null && state.hasActions && model != None)  => nextState(state, 0)._1
   }	  
   
   override def toString: String = qlPolicy.toString + qlSpace.toString

           
   @scala.annotation.tailrec
   private def nextState(st: (QLState[T], Int)): (QLState[T], Int) =  {
  	   val states = qlSpace.nextStates(st._1)
  	   if( states.isEmpty || st._2 >= config.episodeLength) 
  	      st
  	   else
  	      nextState( (states.maxBy(s => model.get.bestPolicy.R(st._1.id, s.id)), st._2+1))
   }
   
	
	private[this] def train(r: Random): Boolean =  {         
		@scala.annotation.tailrec
		def search(st: (QLState[T], Int)): (QLState[T], Int) = {    
			val states = qlSpace.nextStates(st._1)
	        if( states.isEmpty || st._2 >= config.episodeLength ) 
	             (st._1, -1)		
			else {
	            val state = states.maxBy(s => qlPolicy.R(st._1.id, s.id) )
	      
	            if( qlSpace.isGoal(state) )
	                (state, st._2)
	            else {
	               val r = qlPolicy.R(st._1.id, state.id)   
				   val q = qlPolicy.Q(st._1.id, state.id)
				      
				   val nq = q + config.alpha*(r + config.gamma*qlSpace.maxQ(state, qlPolicy) - q)
				   qlPolicy.setQ(st._1.id, state.id,  nq)
				   search((state, st._2+1))
	            }
			}
	    } 	
        r.setSeed(System.currentTimeMillis*Random.nextInt)
        val finalState = search((qlSpace.init(r), 0))
        if( finalState._2 == -1) false
        else
            qlSpace.isGoal(finalState._1)
	}
  }


class QLInput(val from: Int, val to: Int, val reward: Double = 1.0, val prob: Double = 1.0)

		/**
		 * Companion object to the Q-Learning class used to define constants and constructor
		 */
object QLearning {   
   def apply[T](config: QLConfig, numStates: Int, goals: Array[Int], input: Array[QLInput], features: Set[T]): QLearning[T] = {
  	  require(input != null && input.size > 0, "Cannot initialize a Q-learning with undefine input")
      new QLearning[T](config, QLSpace[T](numStates, goals, features, config.neighbors), new QLPolicy[T](numStates, input))
   }
   
   def apply[T](config: QLConfig, numStates: Int, goal: Int, input: Array[QLInput], features: Set[T]): QLearning[T] = 
  	   apply[T](config, numStates, Array[Int](goal), input, features)
  
   protected def check[T](config: QLConfig, qlSpace: QLSpace[T], qlPolicy: QLPolicy[T]): Unit = {
      require(config != null, "Cannot create a Q-Learning model with undefined configuration")
      require(qlSpace != null, "Cannot create a Q-Learning model with undefined state space")
      require(qlPolicy != null, "Cannot create a Q-Learning model with undefined policy")
   }
}





// ----------------------------  EOF --------------------------------------------------------------


    
