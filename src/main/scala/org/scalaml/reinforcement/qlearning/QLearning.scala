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
package org.scalaml.reinforcement.qlearning


import scala.util.Random
import org.scalaml.util.Matrix
import org.scalaml.core.types.ScalaMl._
import org.scalaml.core.design.{Config, PipeOperator}
import QLearning._
import org.scalaml.util.Display
import scala.collection.mutable.ArrayBuffer



		/**
		 * Parameterized class that defines the stateuration parameters for the Q-Learning
		 * @param alpha alpha or learning rate for the Q-Learning algorithm
		 * @param gamma gamma or discount rate for the Q-Learning algorithm
		 * @param maxIters maximum number of iterations allowed during training of Q-Learning
		 * @throws IllegalArgumentException if alpha, gamma or maximum iteration are out of range
		 * 
		 * @author Patrick Nicolas
		 * @since January 19, 2014
		 * @note Scala for Machine Learning
		 */
class QLConfig(val alpha: Double, 
		       val gamma: Double, 
		       val episodeLength: Int, 
		       val numEpisodes: Int, 
		       val minAccuracy: Double, 
		       val neighbors: (Int, Int) =>List[Int]) extends Config {
	require(alpha > 0.0 && alpha < 1.0, s"Cannot initialize QLearning with incorrect alpha $alpha")
	require(gamma > 0.0 && gamma < 1.0, s"Cannot initialize QLearning with incorrect gamma $gamma")
    require(minAccuracy  > 0.4 && minAccuracy <= 1.0, s"Cannot initialize QLearning with incorrect minimum quality condition $minAccuracy")
    val persists = "config/qlearning"
}


class QLModel[T](val bestPolicy: QLPolicy[T], val quality: Double)

		/**
		 * <p>Generic parameterized class that implements the QLearning algorithm. The Q-Learning
		 * model is initialized and trained during the instantiation of the class so it can be 
		 * in the correct state for the run-time prediction. Therefore the class instances have only
		 * two states successfully trained and failed training.<br>
		 * The implementation does not assume that every episode (or training cycle) is successful. 
		 * At completion of the training, the ratio of labels over initial training set is computed.
		 * The client code is responsible to evaluate the quality of the model by testing the ratio
		 * gamma a threshold.</p>
		 * @param state configuration for Q-Learning algorithm
		 * @param qLabels training set input used to build the search space (or model)
		 * @param numStates total number of states in the search space
		 * @throws IllegalArgumentException if the configuration or labels are undefined
		 * 
		 * @author Patrick Nicolas
		 * @since January 22, 2014
		 * @note Scala for Machine Learning
		 */
final class QLearning[T](config: QLConfig, qlSpace: QLSpace[T], qlPolicy: QLPolicy[T]) 
                                   extends PipeOperator[QLState[T], QLState[T]]  {
                                  	 
   require(config != null, "Cannot train Q-Learning with undefined configuration")
   
   val model: Option[QLModel[T]] = {
  	   val r = new Random(System.currentTimeMillis)
  	   val successes = Range(0, config.numEpisodes).foldLeft(0)((s, _) => s + (if(train(r)) 1 else 0))
  	   val accuracy = successes.toDouble/config.numEpisodes
  	   if( accuracy> config.minAccuracy )
  	      Some(new QLModel[T](qlPolicy, accuracy))
  	   else None
   }

    		/**
   		 * <p>Prediction method using Q-Learning model. The model is automatically 
   		 * trained during instantiation of the class. It overrides the
   		 * usual data transformation method (PipeOperator)</p>
   		 * @param data One or several entry as tuple (observations, state)
   		 * @return A tuple (new state, reward) if the training was originally successful and all 
   		 * the rewards associated to the action from the current state can be computed, None otherwise
   		 * @throws IllegalArgumenException if the entry is undefined.
   		 */
      
   override def |> : PartialFunction[QLState[T], QLState[T]] = {
     case state: QLState[T] if(state != null && model != None)  => nextState(state, 0)._1
   }	  
           
   @scala.annotation.tailrec
   private def nextState(stateIter: (QLState[T], Int)): (QLState[T], Int) =  {
  	   val states = qlSpace.nextStates(stateIter._1)
  	   if( states.isEmpty || stateIter._2 >= config.episodeLength) 
  	      stateIter
  	   else
  	      nextState( (states.maxBy(s => model.get.bestPolicy.R(stateIter._1.id, s.id)), stateIter._2+1))
   }
   
	override def toString: String = qlPolicy.toString + qlSpace.toString


	
	private[this] def train(r: Random): Boolean =  {
	   r.setSeed(System.currentTimeMillis*Random.nextInt)
       qlSpace.isGoal(search((qlSpace.init(r), 0))._1)
    }
	
	
	@scala.annotation.tailrec
	private def search(st: (QLState[T], Int)): (QLState[T], Int) = {
		val states = qlSpace.nextStates(st._1)

		if( states.isEmpty || st._2 >= config.episodeLength ) 
             st		
		else {
            val state = states.maxBy(s => qlPolicy.R(st._1.id, s.id) )
      
            if( qlSpace.isGoal(state) )
                (state, st._2)
            else {
               val r = qlPolicy.R(st._1.id, state.id)   
			   val q = qlPolicy.Q(st._1.id, state.id)
			      
			   val nq = q + config.alpha*(r + config.gamma*qlSpace.maxQ(state, qlPolicy) - q)
			   qlPolicy.setQ(st._1.id, state.id,  nq)
			   search((state, st._2))
            }
		}
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
}





// ----------------------------  EOF --------------------------------------------------------------


    
