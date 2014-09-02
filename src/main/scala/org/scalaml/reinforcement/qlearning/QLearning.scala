/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.reinforcement.qlearning


import scala.util.Random
import org.scalaml.util.Matrix
import org.scalaml.core.Types
import QLearning._
import Types.ScalaMl._
import org.scalaml.workflow.PipeOperator


		/**
		 * Parameterized class that defines the configuration parameters for the Q-Learning
		 * @param alpha alpha or learning rate for the Q-Learning algorithm
		 * @param gamma gamma or discount rate for the Q-Learning algorithm
		 * @param maxIters maximum number of iterations allowed during training of Q-Learning
		 * @throws IllegalArgumentException if alpha, gamma or maximum iteration are out of range
		 * 
		 * @author Patrick Nicolas
		 * @since January 19, 2014
		 * @note Scala for Machine Learning
		 */
case class QLConfig(val alpha: Double, val gamma: Double, val maxIters: Int) {
	require(alpha > 0.0 && alpha < 1.0, "Cannot configure QLearning with incorrect alpha " + alpha)
	require(gamma > 0.0 && gamma < 1.0, "Cannot configure QLearning with incorrect gamma " + gamma)
    require(maxIters > 0 && maxIters < QLearning.MAX_ITERS, "Cannot configure QLearning maxIters " + alpha + " is out of bounds")
}



	/**
		 * Parameterized class that defines the training labels for Q-Learning
		 * @param labels List of tuples or episodes (Observations, Goal/state) used in the training phase.
		 * @throws IllegalArgumentException if the list of labels is either undefined or empty
		 * 
		 * @author Patrick Nicolas
		 * @since January 20, 2014
		 */

import QLLabel._
class QLLabel[T <% Double](val labels: List[Episode[T]]) {
   require(labels != null && labels.size > 0, "Cannot traing Q-Learning with undefine labels")	
	
  @inline def size: Int = labels.size
  
  def state0: QLState[T] = labels(0)._2
  def data0: DVector[T] = labels(0)._1
}


	/**
	 * Companion object to QLearningLabel used to define constructors
	 */
object QLLabel {
   type Episode[T] = (DVector[T], QLState[T])
   def apply[T <% Double](labels: List[(DVector[T], QLState[T])]): QLLabel[T] = new QLLabel[T](labels)
}

		/**
		 * <p>Generic parameterized class that implements the QLearning algorithm. The Q-Learning
		 * model is initialized and trained during the instantiation of the class so it can be 
		 * in the correct state for the run-time prediction. Therefore the class instances have only
		 * two states successfully trained and failed training.<br>
		 * The implementation does not assume that every episode (or training cycle) is successful. 
		 * At completion of the training, the ratio of labels over initial training set is computed.
		 * The client code is responsible to evaluate the quality of the model by testing the ratio
		 * agammast a threshold.</p>
		 * @param config configuration for Q-Learning algorithm
		 * @param qLabels training set input used to build the search space (or model)
		 * @param numStates total number of states in the search space
		 * @throws IllegalArgumentException if the configuration or labels are undefined
		 * 
		 * @author Patrick Nicolas
		 * @since January 22, 2014
		 * @note Scala for Machine Learning
		 */
class QLearning[T <% Double](val config: QLConfig, val qLabels: QLLabel[T], val numStates: Int) 
                                   extends PipeOperator[Episode[T], (QLState[T], Double)]  {
                                  	 
   require(config != null, "Cannot traing Q-Learning with undefined configuration")
   require(qLabels != null, "Cannot traing Q-Learning with undefined training labels")
   		// Q-Value matrix
   private val Q = Matrix[Double](numStates, numStates)
   
   		// Reward matrix
   private val R = Matrix[Double](numStates, numStates)
       
   		// SearchSpace as states-actions
   private val statesActions = QLSpace[T](numStates).statesActions
   private val trainQuality = train.toDouble/qLabels.size
   
   		/**
   		 * <p>Prediction method using Q-Learning model. The model is automatically 
   		 * trained during instantiation of the class. It overrides the
   		 * usual data transformation method (PipeOperator)</p>
   		 * @param data One or several entry as tuple (observations, state)
   		 * @return A tuple (new state, reward) if the training was originally successful and all 
   		 * the rewards associated to the action from the current state can be computed, None otherwise
   		 * @throws IllegalArgumenException if the entry is undefined.
   		 */
   override def |>(episode: Episode[T]): Option[(QLState[T], Double)] = {
  	 require(episode != null, "Cannot predict next state with undefined input")
  	 
  	 if( trainQuality > MIN_TRAINING_QUALITY ) {
	  	 statesActions._2.find( _.from.id == episode._2.id)  match {
	  	  	case Some(action) => { 
	  	  	  val bestToState = action.to.maxBy(st => reward(episode._2, st, episode._1))
	  	  	  Some(bestToState, reward(episode._2, bestToState, episode._1))
	  	  	}
	  	  	case None => println("Cannot compute reward for entry " + episode._2.id ); None
	  	  }
  	 }
  	 else
  		 println("Training failed with a ratio " + trainQuality); None
   }
  	  
   
   private def reward(from: QLState[T], to: QLState[T], data: DVector[T]): Double = {
  	  val r = to.V(data) - from.V(data)
  	  R += (from.id, to.id, r)
  	  r
   }
   
   private def train: Int = 
  	   qLabels.labels.foldLeft(0)( (s, episode) => s + {if( search(episode) ) 1 else 0} )


    private def maxQ(state: QLState[T]): Double = 
       statesActions._1.filter( _ != state)
                       .foldLeft(Double.MinValue) ((mx, x) => { 
                          if(Q(state.id, x.id) > mx) Q(state.id, x.id) else mx
                       })
	
	private def nextStates(st: QLState[T]): List[QLState[T]] = {
       statesActions._2.find ( _.from.id == st.id) match {
	     case Some(ac) =>  ac.to
	     case None => List.empty
	   }
	}
    
	
	override def toString: String =  {
	   val desc = new StringBuilder("States\n")
	   desc.append( statesActions._1.foldLeft(new StringBuilder)(( buf, st) => buf.append(st.toString).append("\n")).toString)
	   desc.append("Actions:\n")
	   desc.append( statesActions._2.foldLeft(new StringBuilder)(( buf, ac) => buf.append(ac.toString).append("\n")).toString)
	   desc.toString
	}
	
		/*
		 * Search method for the training of the XCS
		 */
	private[this] def search(episode: (DVector[T], QLState[T])): Boolean =  {
      var prevState = statesActions._1(Random.nextInt(statesActions._1.size-1))
      val data = episode._1
      val goal = episode._2
      
      Range(0, config.maxIters).find( n => {

         val states = nextStates(prevState)
         if( !states.isEmpty ) {
            val state = states.maxBy( m => R(prevState.id, m.id) )
               
            val r = reward(prevState, state, data)   
			val q = Q(prevState.id, state.id)
			      
			val nq = q + config.alpha*(r + config.gamma*maxQ(state) - q)
			Q += (prevState.id, state.id, nq)
			prevState = state 
			state.id == goal.id
         } 
         else 
            false
       }) match {
      	  case Some(_) => false
      	  case None => true
      }
    }
}

		/**
		 * Companion object to the Q-Learning class used to define constants and constructor
		 */
object QLearning {
  def apply[T <% Double](config: QLConfig, qLabels: QLLabel[T], numStates: Int): QLearning[T] = new QLearning(config, qLabels, numStates)

  final val MAX_ITERS = 5000
  final val MIN_TRAINING_QUALITY = 0.85
}


// ----------------------------  EOF --------------------------------------------------------------


    
