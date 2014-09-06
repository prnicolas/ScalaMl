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


import org.scalaml.supervised.nnet._
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import scala.util.Random
import org.scalaml.supervised.nnet.MLPConnection.setNoSoftmax


object MLPConfigEval {
   final val ALPHA = 0.9
   final val ETA = 0.1
   final val GAMMA = 1.0
   final val SIZE_HIDDEN_LAYER = 5
   final val TEST_SIZE = 30
   final val NUM_EPOCHS = 250
   final val NOISE_RATIO = 0.7
   final val EPS = 1e-4
   final val CLASSIFICATION = false
   
   def run(args: Array[String]): Unit =  {
     Console.println("Evaluation of MLP configuration parameters for " + args(1))
     
 	 def f1(x: Double): DblVector = Array[Double](x*(1.0 + NOISE_RATIO*Random.nextDouble), NOISE_RATIO*Random.nextDouble, x*x*(1.0 +  NOISE_RATIO*Random.nextDouble), NOISE_RATIO*Random.nextDouble)
 	 def f2(x: Double): DblVector = Array[Double](NOISE_RATIO*Random.nextDouble, NOISE_RATIO*Random.nextDouble)
 	 
     val x = Array.tabulate(TEST_SIZE)(f1(_))
     val y = Array.tabulate(TEST_SIZE)(f2(_) )
     
      		// Normalization
      val features: XTSeries[DblVector] = XTSeries.normalize(XTSeries[DblVector](x)).get
      val labels = XTSeries.normalize(XTSeries[DblVector](y)).get.toArray
      
      setNoSoftmax
      if(args != null && args.size > 0) {
      	 args(0) match {
      		case "alpha" => eval(-1.0, ETA, features, labels)
      		case "eta" =>  eval(ALPHA, -1, features, labels)
      		case _ => eval(-1.0, -1.0, features, labels)
      	 }
      }
      else 
      	 eval(-1.0, -1.0, features, labels)
   }
   
   private def eval(alpha: Double, eta: Double, features: XTSeries[DblVector], labels: DblMatrix): Unit = 
  	  _eval(alpha, eta, features, labels)
  	 
   private def _eval(alpha: Double, eta: Double, features: DblMatrix, labels: DblMatrix): Unit = {
  	  try {
        (0.001 until 0.01 by 0.002).foreach( x =>  {
        	val _alpha = if(alpha < 0.0)  x else ALPHA
        	val _eta = if(eta < 0.0) x else ETA
        	println("eta  " + _eta + " alpha: " + _alpha)
  	        val config = MLPConfig(_alpha, _eta, GAMMA, Array[Int](SIZE_HIDDEN_LAYER), NUM_EPOCHS, EPS)
     
	        if( MLP[Double](config, features, labels, new MLP.MLPRegression).model == None)
	        	throw new IllegalStateException("Failed to train the model for alpha = " + alpha) })
       }
       catch {
  	 	 case e: IllegalStateException => println(e.toString); None
  	 	 case e: IllegalArgumentException =>   println(e.toString); None
       }
   }
}


// ---------------------------------  EOF --------------------------------------------