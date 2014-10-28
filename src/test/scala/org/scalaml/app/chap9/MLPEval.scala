/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.app.chap9

import org.scalaml.workflow.data.DataSource
import org.scalaml.supervised.nnet._
import org.scalaml.trading.GoogleFinancials
import org.scalaml.core.types.ScalaMl._
import GoogleFinancials._
import org.scalaml.util.Display
import org.apache.log4j.Logger


object MLPEval {
  val path = "resources/data/chap9/"
  val ALPHA = 0.8; 
  val ETA = 0.01
  val NUM_EPOCHS = 250
  val EPS = 1e-3
  val THRESHOLD = 0.12

  final val symbols = Array[String]("FXE", "FXA", "SPY", "GLD", "FXB", "FXF", "FXC", "FXY", "CYB")
  private val logger = Logger.getLogger("MLPEval")
  
  val index: Map[String, Int] = {
  	 import scala.collection.mutable.HashMap
  		 
  	 val _index = new HashMap[String, Int]
  	 symbols.zipWithIndex.foldLeft(new HashMap[String, Int])((mp, si) => {mp.put(si._1, si._2); mp}).toMap
  }

  	
  def run(args: Array[String]): Unit =  {
     Display.show("Evaluation of MLP classifier evaluation without SoftMax conversion", logger)
       
  	 val prices = symbols.map(s => DataSource(path + s +".csv", true, true, 1))
  	                     .map( _ |> GoogleFinancials.close )
  	                     .map( _.toArray)
    
  	 val trends: DblMatrix = prices.map(z => z.drop(1).zip(z.take(z.size-1)).map(xtx => if(xtx._1 - xtx._2 > 0.0) 1.0 else 0.0))
  	 
  	 Display.show(s"\nsize: ${prices(0).size}", logger)
  	 var startTime= System.currentTimeMillis
     var hiddenLayers = Array[Int](4)
     var config = MLPConfig(ALPHA, ETA, hiddenLayers, NUM_EPOCHS, EPS)

     eval(prices, config)
     
     Display.show(s"\nSingle 4 neuron hidden layer: ${(System.currentTimeMillis - startTime)}", logger)
     startTime= System.currentTimeMillis
     
     
     hiddenLayers = Array[Int](4, 4)
     config = MLPConfig(ALPHA, ETA, hiddenLayers, NUM_EPOCHS, EPS)

     eval(prices, config)
     Display.show(s"\nTwo hidden layers of 4 neurons each ${(System.currentTimeMillis - startTime)}", logger)
     startTime= System.currentTimeMillis
     
     hiddenLayers = Array[Int](7, 7)
     config = MLPConfig(ALPHA, ETA, hiddenLayers, NUM_EPOCHS, EPS)
     eval(prices, config)
     Display.show(s"\nTwo hidden layers of 7 neurons each ${(System.currentTimeMillis - startTime)}", logger)
           
     hiddenLayers = Array[Int](8, 5, 6)
     config = MLPConfig(ALPHA, ETA, hiddenLayers, NUM_EPOCHS, EPS)
     eval(prices, config)
     Display.show(s"\nThree hidden layers of 7 neurons each ${(System.currentTimeMillis - startTime)}", logger)
  }
  
 private def eval(obs: DblMatrix, config: MLPConfig) {
     accuracy(Array[String]("FXY", "FXC", "GLD", "FXA"), obs, config) match {
  		 case Some(acc) => Display.show(s"Accuracy: $acc", logger)
  		 case None => Display.error("Could not compute the accuracy", logger)
  	 }    
  	 
  	 accuracy(Array[String]("FXE", "FXF", "FXB", "CYB"), obs, config) match {
  		 case Some(acc) => Display.show(s"Accuracy: $acc", logger)
  		 case None => Display.error("Could not compute the accuracy", logger)
  	 } 
  	 
  	 accuracy(Array[String]("FXE", "FXC", "GLD", "FXA", "FXY", "FXB"), obs, config) match {
  		 case Some(acc) => Display.show(s"Accuracy: $acc", logger)
  		 case None => Display.error("Could not compute the accuracy", logger)
  	 }    
  	 
  	 accuracy(Array[String]("FXC", "FXY", "FXA"), obs, config) match {
  		 case Some(acc) => Display.show(s"Accuracy: $acc", logger)
  		 case None => Display.error("Could not compute the accuracy", logger)
  	 } 
  	 
  	  accuracy(Array[String]("CYB", "GLD", "FXY"), obs, config) match {
  		 case Some(acc) => Display.show(s"Accuracy: $acc", logger)
  		 case None => Display.error("Could not compute the accuracy", logger)
  	 } 
  	 
  	 accuracy(symbols, obs, config) match {
  		 case Some(acc) => Display.show(s"Accuracy: $acc", logger)
  		 case None => Display.error("Could not compute the accuracy", logger)
  	 } 
  }
 
  private def accuracy(symbols: Array[String], prices: DblMatrix, config: MLPConfig): Option[Double] = {  	 
     val obs: DblMatrix = symbols.map( sym => index.get(sym).get).map( prices( _ ) )

     val features = obs.drop(1).transpose
     val target = Array[DblVector](obs(0)).transpose

     
     implicit val mlpObjective = new MLP.MLPBinClassifier
  	 val classifier = MLP[Double](config, features, target)
  	 classifier.accuracy(THRESHOLD)
   }
  
  
  private def toString(symbols: Array[String]): String = 
  	 new StringBuilder(symbols.drop(1).foldLeft(new StringBuilder)((b,s) => b.append(s).append(" ")).toString)
  	     .append("=> ")
  	       .append(symbols(0)).toString
}


object MLPEvalApp extends App {
	MLPEval.run(null)
}

// -------------------------------------  EOF ----------------------------------------------------------