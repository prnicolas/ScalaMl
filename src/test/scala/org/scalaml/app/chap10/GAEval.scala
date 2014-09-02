/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.app.chap10

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.{TradingStrategy, Signal}
import org.scalaml.ga.{Operator, GASolver, GAConfig, Population, Chromosome}
import scala.collection.mutable.ListBuffer

/**
 *  @author Patrick Nicolas
 *  @date Aug 10, 2014
 *  @project Book
 */
object GAEval  {
   import org.scalaml.trading.YahooFinancials._
   
   final val path = "resources/data/chap10/"
   final val MAX_POPULATION_SIZE = 250
   final val XOVER = 0.62
   final val MUTATE = 0.08
   final val MAX_NUM_ITERS = 400
	   
   def run: Unit = { 
	   val extractor = toDouble(ADJ_CLOSE) :: 
	                   % (HIGH, LOW) ::
	                   toDouble(VOLUME) ::
	                   vol ::
	                   % (OPEN, CLOSE) ::
	                   List[Array[String] =>Double]()
	                   
	   val data = DataSource.listSymbols(path)
	                        .map(s => DataSource(path + s, true, true, 1) |> extractor)
	                        .map( _.get)
	                   
	   implicit val digitize = (x: Double) => (x*1024).toInt 
	   val signals1 = Signal("Rel volume", 0.7, Operator.LESS_THAN) ::
	                  Signal("Rel volatility", 0.6, Operator.GREATER_THAN) :: 
	                  List[Signal]()
	                 
	   val signals2 = Signal("Rel volatility", 0.8, Operator.GREATER_THAN) ::
	                  Signal("Rel volatility/volume", 0.1, Operator.GREATER_THAN) ::
	                  List[Signal]()    
	        
	   val signals3 = Signal("Open-Close", 0.7, Operator.GREATER_THAN) ::
	                  Signal("Rel volatility/volume", 0.2, Operator.GREATER_THAN) ::
	                  List[Signal]()
	
	   val strategies = new ListBuffer[Chromosome[Signal]]
	   strategies.append(Chromosome[Signal](signals1))
	   strategies.append(Chromosome[Signal](signals2))
	   strategies.append(Chromosome[Signal](signals3))
	       
	   
	   val initialPopulation = new Population[Signal](MAX_POPULATION_SIZE, strategies)
	   val gaSolver = GASolver[Signal](GAConfig(MAX_POPULATION_SIZE, XOVER, MUTATE, MAX_NUM_ITERS), initialPopulation)
	   
	   val scoring = (chr: Chromosome[Signal]) => { chr.code.foldLeft(0.0)((sum, s) => sum + s.score(s.value) )}
	   gaSolver.search(scoring)
   }

}

// ----------------  EOF ------------------------------------