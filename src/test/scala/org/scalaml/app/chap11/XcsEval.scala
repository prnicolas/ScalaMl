package org.scalaml.app.chap11

import org.scalaml.trading.StrategyFactory

object XcsEval {
	/*
   val factory = new StrategyFactory(2)
	   factory +=  ("Delta_volume", 1.1, GREATER_THAN, deltaVolume, deltaPrice)
	   factory +=  ("Rel_volatility", 1.3, GREATER_THAN, relVolatility.get.drop(1), deltaPrice)
	   factory +=  ("Rel_close-Open", 0.8, LESS_THAN, relCloseOpen.get.drop(1), deltaPrice)
	   factory +=  ("Delta_volatility", 0.9, GREATER_THAN, deltaVolatility, deltaPrice)
	
	   val population = Population[Signal]((factory.strategies.size <<4), factory.strategies)
	         // compute the score for the entire time series
	   population.display(s"Initial population: $population\n")
	   
	   val CUTOFF_SLOPE = -0.003
	   val CUTOFF_INTERCEPT = 1.003
	   val softLimit = (n: Int) => CUTOFF_SLOPE*n + CUTOFF_INTERCEPT
	   val config = GAConfig(XOVER, MU, MAX_CYCLES, softLimit )
	   
	   val scoring = (chr: Chromosome[Signal]) =>  {
	  	  val signals: List[Gene] = chr.code
		  chr.unfitness = signals.foldLeft(0.0)((sum, s) => sum + s.score)
	   }
	   * 
	   */
}

// --------------------------------  EOF --------------------------------