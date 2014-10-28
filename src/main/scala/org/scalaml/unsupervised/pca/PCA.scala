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
package org.scalaml.unsupervised.pca

import org.scalaml.core.XTSeries
import org.apache.commons.math3.linear._
import org.apache.commons.math3.stat.correlation.Covariance
import org.apache.commons.math3.exception.{MathIllegalArgumentException, MaxCountExceededException, MathArithmeticException, DimensionMismatchException}
import org.scalaml.core.types
import org.scalaml.core.design.PipeOperator
import types.ScalaMl._
import org.scalaml.util.Display
import org.apache.log4j.Logger
import scala.util.{Try, Success, Failure}

		/**
		 * <p>Generic class that implements the Principal Component Analysis technique. The
		 * extraction of the principal components (Eigenvectors). The class is parameterized
		 * as a view bound to Double. The purpose of the class is to compute the covariance
		 * matrix and the eigenvalues (normalized values ordered by decreasing order). The features
		 * (or variables) associated with a low eigenvalue are eliminated, reducing the dimension 
		 * of the model and the complexity of any future supervised learning algorithm.</p>
		 * @constructor Instantiate a principal component analysis algorithm as a data transformation of type PipeOperator
		 * 
		 * @author Patrick Nicolas
		 * @since February 26, 2014
		 * @note Scala for Machine Learning
		 */
final class PCA[T <% Double] extends PipeOperator[XTSeries[Array[T]], (DblMatrix, DblVector)] {
  import types.CommonMath._, XTSeries._
  private val logger = Logger.getLogger("PCA")	   
		/**
		 * <p>Data transformation that implements the extraction of the principal components
		 * from a time series. The methods uses the Apache Commons Math library to compute
		 * eigenvectors and eigenvalues. All the exceptions thrown by the Math library during 
		 * the manipulation of matrices are caught in the method.</p>
		 * @param xt time series of dimension > 1 
		 * @throws MatchError if the input time series is undefined or have no elements
   		 * @return PartialFunction of time series of elements of type T as input to the Principal Component Analysis and tuple Covariance matrix and vector of eigen values as output
		 */
	override def |> : PartialFunction[XTSeries[Array[T]], (DblMatrix, DblVector)] = {
	  case xt: XTSeries[Array[T]] if(xt != null && xt.size > 0) => {
	    Try {
	      zScoring(xt) match {
	  	  	 case Some(observation) => {
	  	  		 val obs: DblMatrix = observation  
	  	  		 val covariance = new Covariance(obs).getCovarianceMatrix
		         val transform = new EigenDecomposition(covariance)
	             val eigenVectors = transform.getV
	             val eigenValues = new ArrayRealVector(transform.getRealEigenvalues)
	             (obs.multiply(eigenVectors).getData, eigenValues.toArray)
	  	  	  }
	  	  	  case None => throw new Exception("zScoring failed")
	  	   }
	    } match {
	       case Success(eigenResults) => eigenResults
	       case Failure(e) => Display.error("PCA.|> zScoring ", logger, e); null
	    }
	  }
	}
}


//--------------------------------  EOF -------------------------------------------------------------------------