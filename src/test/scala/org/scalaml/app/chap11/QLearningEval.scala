/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.app.chap11

import org.scalaml.reinforcement.qlearning._
// import org.scalaml.plots.{ScatterPlot, LinePlot, LightPlotTheme}
import org.scalaml.workflow.data.DataSource
import org.scalaml.core.XTSeries
import org.scalaml.trading.YahooFinancials
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.util.{Counter, NumericAccumulator, Display}
import org.scalaml.app.Eval

import org.apache.log4j.Logger

import scala.collection.mutable.{ArrayBuffer, ListBuffer}



	 
object QLearningEval extends Eval {
	import YahooFinancials._
  
	val name: String = "QLearningEval"
	val maxExecutionTime: Int = 7000
	  
	private val logger = Logger.getLogger(name)
	      
	private val stockPricePath = "resources/data/chap11/IBM.csv"
	private val optionPricePath = "resources/data/chap11/IBM_O.csv"
	private val STRIKE_PRICE = 190.0
	 
	private val MIN_TIME_EXPIRATION = 6
	private val FUNCTION_APPROX_STEP = 3
	private val ALPHA = 0.4
	private val DISCOUNT = 0.6
	private val EPISODE_LEN = 35
	private val NUM_EPISODES = 60
	private val MIN_ACCURACY = 0.55
    
		/** <p>Execution of the scalatest for <b>QLearning</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = { 
		Display.show(s"\n\n *****  test#${Eval.testCount} $name Evaluation of the Q-learning algorithm", logger)
       
		val src = DataSource(stockPricePath, false, false, 1)
		val ibmOption = new OptionModel("IBM", STRIKE_PRICE, src, MIN_TIME_EXPIRATION, FUNCTION_APPROX_STEP)
  	   
		val optionSrc = DataSource(optionPricePath, false, false, 1)
		optionSrc.extract match {
			case Some(v) => {
				val qLearning = createModel(ibmOption, v)
				if( qLearning.model != None)
					Display.show(s"$name QLearning model ${qLearning.model.get.toString}", logger)
				else
					Display.error(s"$name Failed to create a Q-learning model", logger)
			}
			case None => Display.error(s"$name Failed extracting option prices", logger)
		}
	}
    
	
	private def createModel(ibmOption: OptionModel, oPrice: DblVector): QLearning[Array[Int]] = {
		val fMap = ibmOption.approximate(oPrice)
      
		val input = new ArrayBuffer[QLInput]
		val profits = fMap.values.zipWithIndex
		profits.foreach(v1 => 
			profits.foreach( v2 => 
				input.append(new QLInput(v1._2, v2._2, v1._1 - v2._1)))
		)
   	      
		val goal = input.maxBy( _.reward).to  
		Display.show(s"$name Goal state: ${goal.toString}", logger)
      
	  
		val config = QLConfig(ALPHA, DISCOUNT, EPISODE_LEN, NUM_EPISODES, getNeighbors)
		QLearning[Array[Int]](config, fMap.size, goal, input.toArray, fMap.keySet)
	}
    
        	// List the neighbors that are allowed 
	private val RADIUS = 4
	val getNeighbors = (idx: Int, numStates: Int) => {
        
		def getProximity(idx: Int, radius: Int): List[Int] = {
			val idx_max = if(idx + radius >= numStates) numStates-1 else idx+ radius
			val idx_min = if(idx < radius) 0 else idx - radius
			Range(idx_min, idx_max+1).filter( _ != idx)
									.foldLeft(List[Int]())((xs, n) => n :: xs)
		}
		getProximity(idx, RADIUS).toList
	}
}


// ------------------------------------  EOF ----------------------------------