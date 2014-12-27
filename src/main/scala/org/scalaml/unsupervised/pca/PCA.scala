/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.unsupervised.pca

import scala.util.{Try, Success, Failure}

import org.apache.log4j.Logger
import org.apache.commons.math3.linear._
import org.apache.commons.math3.stat.correlation.Covariance

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.{CommonMath, ScalaMl}
import org.scalaml.core.Design.PipeOperator
import org.scalaml.util.DisplayUtils
import ScalaMl._


		/**
		 * <p>Generic class that implements the Principal Component Analysis technique. The
		 * extraction of the principal components (Eigenvectors). The class is parameterized
		 * as a view bound to Double. The purpose of the class is to compute the covariance
		 * matrix and the eigenvalues (normalized values ordered by decreasing order).<br> The features
		 * (or variables) associated with a low eigenvalue are eliminated, reducing the dimension 
		 * of the model and the complexity of any future supervised learning algorithm.</p>
		 * @constructor Instantiate a principal component analysis algorithm as a data transformation 
		 * of type PipeOperator
		 * 
		 * @author Patrick Nicolas
		 * @since February 26, 2014
		 * @note Scala for Machine Learning Chapter 4 Unsupervised learning / Principal Components Analysis
		 */
final class PCA[T <% Double] extends PipeOperator[XTSeries[Array[T]], (DblMatrix, DblVector)] {
	import CommonMath._, XTSeries._

	private val logger = Logger.getLogger("PCA")	   
		/**
		 * <p>Data transformation that implements the extraction of the principal components
		 * from a time series. The methods uses the Apache Commons Math library to compute
		 * eigenvectors and eigenvalues. All the exceptions thrown by the Math library during 
		 * the manipulation of matrices are caught in the method.</p>
		 * @param xt time series of dimension > 1 
		 * @throws MatchError if the input time series is undefined or have no elements
		 * @return PartialFunction of time series of elements of type T as input to the Principal 
   	 * Component Analysis and tuple Covariance matrix and vector of eigen values as output
		 */
	override def |> : PartialFunction[XTSeries[Array[T]], (DblMatrix, DblVector)] = {
		case xt: XTSeries[Array[T]] if( !xt.isEmpty ) => {
			Try {
				// Compute the zScore of the time series (1)	
				zScoring(xt).map(observation => { 
					
					// Forces a conversion
					val obs: DblMatrix = observation
						// Compute the covariance matrix related to the observations in the time series (3)
					val covariance = new Covariance(obs).getCovarianceMatrix
						
						// Create a Eigenvalue and Eigenvectors decomposition (4)
					val transform = new EigenDecomposition(covariance)
						
						// Retrieve the principal components (or direction)
					val eigenVectors = transform.getV
						
						// Retrieve the eigen values
					val eigenValues = new ArrayRealVector(transform.getRealEigenvalues)
					
					val cov = obs.multiply(eigenVectors).getData
						// Return the tuple (Covariance matrix, Eigenvalues)
					(cov, eigenValues.toArray)
				})
			} 
				// Return the tuple (Empty Covariance matrix, Empty Eigenvalue vector)
				// if an exception is thrown.
			match {
				case Success(eigenResults) => eigenResults.getOrElse((Array.empty, Array.empty))
				case Failure(e) => {
					DisplayUtils.error("PCA.|> zScoring ", logger, e)
					(Array.empty, Array.empty)
				}
			}
		}
	}
}

//--------------------------------  EOF -------------------------------------------------------------------------