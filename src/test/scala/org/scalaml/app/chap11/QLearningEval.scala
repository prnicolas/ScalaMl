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
package org.scalaml.app.chap11

import org.scalaml.reinforcement.qlearning._
import org.scalaml.plots.{ScatterPlot, LinePlot, LightPlotTheme}
import org.scalaml.workflow.data.DataSource
import org.scalaml.core.XTSeries
import org.scalaml.trading.YahooFinancials
import YahooFinancials._
import org.scalaml.core.types.ScalaMl.DblVector
import org.apache.log4j.Logger
import org.scalaml.util.Display
import scala.collection.mutable.ArrayBuffer
import org.scalaml.util.{Counter, NumericAccumulator}
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import org.scalaml.app.Eval

	 
object QLearningEval extends Eval {
	val name: String = "QLearningEval"
    val stockPricePath = "resources/data/chap11/IBM.csv"
    val optionPricePath = "resources/data/chap11/IBM_O.csv"
	 
    val MIN_TIME_EXPIRATION = 6
    val FUNCTION_APPROX_STEP = 3
    val ALPHA = 0.4
    val DISCOUNT = 0.6
    val EPISODE_LEN = 15
    val NUM_EPISODES = 24
    val MIN_ACCURACY= 0.8
    
    private val logger = Logger.getLogger(name)
    
    def run(args: Array[String]): Int = { 
       val src = DataSource(stockPricePath, false, false, 1)
       val ibmOption = new OptionModel("IBM", 190.0, src, MIN_TIME_EXPIRATION, FUNCTION_APPROX_STEP)
  	   
  	   val optionSrc = DataSource(optionPricePath, false, false, 1)
       optionSrc.extract match {
          case Some(v) => {
          	val model = initializeModel(ibmOption, v)
          	Display.show(s"$name QLearning model ${model.toString}", logger)
          }
      	  case None => Display.error(s"$name Failed extracting option prices", logger)
       }
    }
    
	
    def initializeModel(ibmOption: OptionModel, oPrice: DblVector): QLearning[Array[Int]] = {
      val fMap = ibmOption.approximate(oPrice)
      val input = new ArrayBuffer[QLInput]
      val profits = fMap.values.zipWithIndex
      profits.foreach(v1 => 
      	  	 profits.foreach( v2 => 
      	  		  input.append(new QLInput(v1._2, v2._2, v1._1 - v2._1)))
      )
   	      
      val goal = input.maxBy( _.reward).to
	          
	  val getNeighbors = (idx: Int, numStates: Int) => {
		val rows = idx/numStates
		val cols = if( rows == 0) idx else (idx % (rows*numStates))
		val _toList = new ListBuffer[Int]
		if(cols > 0) _toList.append(cols-1)
		
		if(cols < numStates-1) _toList.append(cols+1)
		_toList.append(cols)
		_toList.toList	
      }
	  
      val config = new QLConfig(ALPHA, DISCOUNT, EPISODE_LEN, NUM_EPISODES, MIN_ACCURACY, getNeighbors)
	  QLearning[Array[Int]](config, fMap.size, goal, input.toArray, fMap.keySet)
   }
}


object AA extends App {
    QLearningEval.run(Array.empty)
}



// ------------------------------------  EOF ----------------------------------