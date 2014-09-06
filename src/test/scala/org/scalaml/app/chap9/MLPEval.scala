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
package org.scalaml.app.chap9

import org.scalaml.workflow.data.DataSource
import org.scalaml.supervised.nnet._
import org.scalaml.trading.GoogleFinancials
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.supervised.nnet.MLPConnection.setNoSoftmax


import GoogleFinancials._
object MLPEval {
  final val path = "resources/data/chap9/"
  final val ALPHA = 0.8; val ETA = 0.015
  final val GAMMA = 1.0
  final val NUM_EPOCHS = 200
  final val EPS = 1e-4
  final val THRESHOLD = 0.35
  final val ACTIVATION = (x:Double) => Math.tanh(x) 	
  
  final val symbols = Array[String]("FXE", "FXA", "SPY", "GLD", "FXB", "FXF", "FXC", "FXY", "CYB")
  	 
  val index: Map[String, Int] = {
  	 import scala.collection.mutable.HashMap
  		 
  	 val _index = new HashMap[String, Int]
  	 symbols.zipWithIndex.foldLeft(new HashMap[String, Int])((mp, si) => {mp.put(si._1, si._2); mp}).toMap
  }

  	
  def run(args: Array[String]): Unit =  {
     Console.println("Evaluation of MLP classifier evaluation without SoftMax conversion")
       
  	 val prices = symbols.map(s => DataSource(path + s +".csv", true, true, 1))
  	                   .map( _ |> GoogleFinancials.close )
  	                   .map( _.get.toArray)
    
     setNoSoftmax
     var hiddenLayers = Array[Int](4)
     var config = MLPConfig(ALPHA, ETA, GAMMA,  hiddenLayers, NUM_EPOCHS, EPS, ACTIVATION)
     println("\nSingle 4 neuron hidden layer")
     eval(prices, config)
 
     hiddenLayers = Array[Int](4, 4)
     config = MLPConfig(ALPHA, ETA, GAMMA,  hiddenLayers, NUM_EPOCHS, EPS, ACTIVATION)
     println("\nTwo hidden layers of 4 neurons each")
     eval(prices, config)
     
     hiddenLayers = Array[Int](7, 7)
     config = MLPConfig(ALPHA, ETA, GAMMA,  hiddenLayers, NUM_EPOCHS, EPS, ACTIVATION)
     println("\nTwo hidden layers of 7 neurons each")
     eval(prices, config)
  }
  
 private def eval(obs: DblMatrix, config: MLPConfig) {
     accuracy(Array[String]("FXY", "FXC", "GLD", "FXA"), obs, config) match {
  		 case Some(acc) => println(acc)
  		 case None => println("Could not compute the accuracy")
  	 }    
  	 
  	 accuracy(Array[String]("FXE", "FXF", "FXB", "CYB"), obs, config) match {
  		 case Some(acc) => println(acc)
  		 case None => println("Could not compute the accuracy")
  	 } 
  	 
  	 accuracy(Array[String]("FXE", "FXC", "GLD", "FXA", "FXY", "FXB"), obs, config) match {
  		 case Some(acc) => println(acc)
  		 case None => println("Could not compute the accuracy")
  	 }    
  	 
  	 accuracy(Array[String]("FXC", "FXY", "FXA"), obs, config) match {
  		 case Some(acc) => println(acc)
  		 case None => println("Could not compute the accuracy")
  	 } 
  	 
  	  accuracy(Array[String]("CYB", "GLD", "FXY"), obs, config) match {
  		 case Some(acc) => println(acc)
  		 case None => println("Could not compute the accuracy")
  	 } 
  	 
  	 accuracy(symbols, obs, config) match {
  		 case Some(acc) => println(acc)
  		 case None => println("Could not compute the accuracy")
  	 } 
  }
 
  private def accuracy(symbols: Array[String], prices: DblMatrix, config: MLPConfig): Option[Double] = {  	 
     val obs: Array[DblVector] = symbols.map( sym => index.get(sym).get).map( prices( _ ) )

     val features = obs.drop(1).transpose
     val target = Array[DblVector](obs(0)).transpose
  	 val classifier = MLP[Double](config, features, target, new MLP.MLPClassification(target))
  	 classifier.accuracy(THRESHOLD)
   }
  
  private def toString(symbols: Array[String]): String = 
  	 new StringBuilder(symbols.drop(1).foldLeft(new StringBuilder)((b,s) => b.append(s).append(" ")).toString)
  	     .append("=> ")
  	       .append(symbols(0)).toString
}

// -------------------------------------  EOF ----------------------------------------------------------