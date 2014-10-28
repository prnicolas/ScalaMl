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
package org.scalaml.reinforcement.qlearning

import org.scalaml.util.Matrix
import org.scalaml.core.design.Model



object QLDataVar extends Enumeration {
  type QLDataVar = Value
  val REWARD, PROB, VALUE = Value
}


import QLDataVar._
class QLData {
	var reward: Double = 1.0
	var probability: Double = 1.0
	var value: Double = 0.0
	
	@inline
	final def estimate: Double = value*probability
	
	final def value(varType: QLDataVar): Double = varType match {
	   case REWARD => reward
	   case PROB => probability
	   case VALUE => value
	}
	
	override def toString: String =
		new StringBuilder(s"\nValue= $value")
	              .append(s" Reward= $reward")
	              .append(s" Probability= $probability").toString
}



class QLPolicy[T](numStates: Int, input: Array[QLInput]) extends Model {
   val persists = "models/qlearning"
     
   val qlData = {
  	  val data = Array.tabulate(numStates)(v => Array.fill(numStates)(new QLData))
  	  input.foreach(in => {  
  	     data(in.from)(in.to).reward = in.reward
  	     data(in.from)(in.to).probability = in.prob
      })
      data
   }

   
   def setQ(from: Int, to: Int, value: Double): Unit = qlData(from)(to).value = value
   final def Q(from: Int, to: Int): Double = qlData(from)(to).value
   final def EQ(from: Int, to: Int): Double = qlData(from)(to).estimate
   
   final def R(from: Int, to: Int): Double = qlData(from)(to).reward
   
   final def P(from: Int, to: Int): Double = qlData(from)(to).probability
   
   
   override def toString: String = 
  	  new StringBuilder("\nQ-Policy: Q-Value\n")
  	                     .append(show(VALUE))
  	                     .append("\nQ-Policy: Reward\n")
  	                     .append(show(REWARD)).toString

   def show(varType: QLDataVar): String = { 
  	  val buf = new StringBuilder
      Range(1, numStates).foreach(i => {
          val line = qlData(i).zipWithIndex
          		              .foldLeft(new StringBuilder)((b, qj) => 
          	                     b.append(f"${qj._1.value(varType)}%1.2f,") )
          line.setCharAt(line.size-1, '\n')
          	    
          buf.append(line.toString)
      })
      buf.toString
    } 
}




// -----------------------------  EOF -----------------------------------