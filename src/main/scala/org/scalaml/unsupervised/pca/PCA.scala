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
package org.scalaml.unsupervised.pca

import org.scalaml.core.XTSeries
import org.apache.commons.math3.linear._
import org.apache.commons.math3.stat.correlation.Covariance
import org.apache.commons.math3.exception.{MathIllegalArgumentException, MaxCountExceededException, MathArithmeticException, DimensionMismatchException}
import org.scalaml.core.Types
import org.scalaml.workflow.PipeOperator



		/**
		 * <p>Generic class that implements the Principal Component Analysis technique. The
		 * extraction of the principal components (Eigenvectors). The class is parameterized
		 * as a view bound to Double. The purpose of the class is to compute the covariance
		 * matrix and the eigenvalues (normalized values ordered by decreasing order). The features
		 * (or variables) associated with a low eigenvalue are eliminated, reducing the dimension 
		 * of the model and the complexity of any future supervised learning algorithm.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since February 26, 2014
		 * @note Scala for Machine Learning
		 */
import Types.ScalaMl._
final class PCA[T <% Double] extends PipeOperator[XTSeries[Array[T]], (DblMatrix, DblVector)] {
	import Types.ScalaMl._, Types.CommonMath._, XTSeries._
		   
		/**
		 * <p>Data transformation that implements the extraction of the principal components
		 * from a time series. The methods uses the Apache Commons Math library to compute
		 * eigenvectors and eigenvalues. All the exceptions thrown by the Math library during 
		 * the manipulation of matrices are caught in the method.</p>
		 * @param xt time series of dimension > 1 
		 * @throws IllegalArgumentException if the time series is undefined or has a dimension of 1
		 * @return tuple Covariance matrix and vector of eigen values if the computation succeeds, None otherwise.
		 */
	override def |> (xt: XTSeries[Array[T]]): Option[(DblMatrix, DblVector)] = {
	   require( xt != null && xt.size > 0, "Cannot compute PCA for undefined data set")
	   require(xt(0).size > 1, "No need to compute PCA for a single variable time series")
	   
	   try {
	  	 	// needs to apply the z transform to the original series
	  	    // the assignment forces a conversion to internal type DblMatrix
	  	   zScoring(xt) match {
	  	  	 case Some(observation) => {
	  	  		 val obs: DblMatrix = observation  
	  	  		 val covariance = new Covariance(obs).getCovarianceMatrix
		         val transform = new EigenDecomposition(covariance)
	             val eigenVectors = transform.getV
	             val eigenValues = new ArrayRealVector(transform.getRealEigenvalues)
	             Some(obs.multiply(eigenVectors).getData, eigenValues.toArray)
	  	  	  }
	  	  	 case None => None
	  	   }
	   }
	   catch {
		   case e: MathIllegalArgumentException => println("Covariance matrix: " + e.toString); None
		   case e: MaxCountExceededException => println("EigenDecomposition: " + e.toString); None
		   case e: MathArithmeticException => println("EigenDecomposition: " + e.toString); None
		   case e: DimensionMismatchException => println("Matrix multiplication " + e.toString); None
		   case e: Exception => println("Generic exception " + e.toString); None
	   }
	}
}


//--------------------------------  EOF -------------------------------------------------------------------------