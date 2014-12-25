/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.chap4

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.DataSource
import org.scalaml.unsupervised.pca.PCA
import org.scalaml.util.DisplayUtils
import org.scalaml.app.Eval

		/**
		 * <p><b>Purpose:</b> Singleton to evaluate the Principal Components Algorithm to 
		 * extract principal components from a set of observations consisting of the following 
		 * corporate financial fundamental metrics:<br>
		 * PE: Price Earning ratio<br>
		 * PS: Price sales ratio<br>
		 * PB: Price book ratio<br>
		 * ROE: Return on Equity<br>
		 * OM: Operational margin.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since February 25, 2014
		 * @note Scala for Machine Learning Chapter 4: Unsupervised learning $Principal Components Analysis
		 */
object PCAEval extends UnsupervisedLearningEval {	
	import org.apache.log4j.Logger
	import XTSeries._, YahooFinancials._, ScalaMl._
  
		/**
		 * Name of the evaluation 
		 */
	val name: String = "PCAEval"
	
	// Symbol, PE/PS/PB/ROE/OM
	private val data = Array[(String, Array[Double])] (
		("QCOM", Array[Double](20.8, 5.32, 3.65, 17.65,29.2)),
		("IBM", Array[Double](13, 1.22, 12.2, 88.1,19.9)),  
		("BAC", Array[Double](21, 2.0, 0.78, 4.12,24.2)),    
		("AA", Array[Double](21.7, 0.7, 1.4, -16.3, 5.1)),
		("ADM", Array[Double](22.3, 0.33, 1.47, 6.9, 2.4)),
		("AET", Array[Double](12.7, 0.54, 1.8, 15.6, 7.6)),
		("AFAM", Array[Double](24.8, 0.57, 0.94, 4.0, 4.1)),
		("AHS", Array[Double](18.6, 0.57, 2.6, 16.4, 6.5)),
		("UNH", Array[Double](13.8, 0.63, 2.4, 17.1, 7.8)),
		("THC", Array[Double](17.4, 0.38, 4.9, 7.9, 7.1)),
		("BK", Array[Double](19.4, 2.5, 1.0, 5.8, 27.4)),
		("JPM", Array[Double](13.7, 2.2, 1.0, 7.8,38.1)),
		("WFC", Array[Double](12.2, 3.1, 1.5, 13.5,43.7)),
		("HCA", Array[Double](14.6, 0.63, 2.5, 17.4, 9.7)),
		("FBP", Array[Double](12.8, 2.2, 0.91, -12.1,29.7)),
		("USB", Array[Double](13.4, 4.2, 2.0, 13.7, 43.4)),
		("FFBC", Array[Double](20.4, 3.3, 1.4, 7.1, 26.7)),
		("CAH", Array[Double](16.4, 0.3, 3.5, 5.8, 2.1)),
		("DVA", Array[Double](23.5, 1.25, 3.3, 15.1, 15.7)),
		("FHN", Array[Double](13.9, 2.35, 1.2, 1.6, 9.3)),
		("FB", Array[Double](96.7, 19.4, 9.9, 11.7, 37.1)),
		("TWTR", Array[Double](208, 37.9, 8.9, -34, -96)),
		("YELP", Array[Double](231, 21.1, 9.6, -3.1,-4.7)),
		("ANGI", Array[Double](67.3, 3.4, 7.9, -16.7, -11.3)),
		("LNKD", Array[Double](771, 13.6, 7.9, 1.5, 3.1)),
		("TSLA", Array[Double](101.2, 12.9, 36.1, -18.7, -3.8)),
		("CYH", Array[Double](21.6, 0.28, 1.1, 6.6, 7.9)),
		("PCLN", Array[Double](37.6, 9.5, 9.2, 34.8, 35.9)),
		("CVS", Array[Double](19.7, 0.68, 2.3, 12.2, 6.3)),
		("FISV", Array[Double](23.7, 3.0, 4.1, 18.6, 22.7)),
		("DOW", Array[Double](13.3, 1.0, 2.5, 19.3, 8.0)),
		("K", Array[Double](13.4, 1.6, 6.7, 59.8, 21.3))
	)
  
  		/**
		 * <p>Execution of the scalatest for <b>PCA</b> class
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	override def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Evaluation of Principal Component Analysis", logger)
  	  
		import scala.util.{Try, Success, Failure}
		val pca = new PCA[Double]
		  
		Try(pca |> XTSeries[DblVector](data.map( _._2.take(3)) ) ) match {
			case Success(covariance) => 
					DisplayUtils.show(s"$name Results\n${this.toString(covariance)}", logger)
			case Failure(e) => failureHandler(e)
		}
	}
   
	private def toString(covariance: (DblMatrix, DblVector)): String = {
		val buf = new StringBuilder(s"\n$name PCA Eigenvalues:\n")
		
		buf.append(s"${covariance._2.mkString(" ,")}\n\n$name PCA Covariance matrix\n")
		covariance._1.foreach(r => buf.append(s"${r.mkString(" ")}\n") )
				
		buf.toString
	}
}


// -----------------------------------  EOF ---------------------------------------------------