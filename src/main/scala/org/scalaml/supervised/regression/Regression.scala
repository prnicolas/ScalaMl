/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99.1
 */
package org.scalaml.supervised.regression

import scala.util.{Try, Success, Failure}

import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl.{DblVector, DblArray}
import org.scalaml.util.LoggingUtils._



		/**
		 * Generic trait for the regression models in the library
		 */
private[scalaml] trait Regression extends Monitor[Double] {

	protected val logger: Logger = Logger.getLogger("Regression")

  	/**
  	 * The model is created during the instantiation of the LogisicRegression classifier
		 * through training. It is set as None if the model could not be trained.
		 */
	protected[this] val model: Option[RegressionModel] = training
	
		/**
		 * Access the weights of the logistic regression model.
		 * @return Vector of weights if the model has been successfully trained, None otherwise.
		 */
	final def weights: Option[DblArray] = model.map( _.weights)
	
		/**
		 * Access the residual sum of squares of the logistic regression model.
		 * @return rss if the model has been successfully trained, None otherwise.
		 */
	final def rss: Option[Double] = model.map(_.rss)
	
	@inline
	final def isModel: Boolean = model.isDefined
	
	protected def train: RegressionModel
  
	private def training: Option[RegressionModel] = Try( train ) match {
		case Success(_model) => Some(_model)
		case Failure(e) => e match {
			case ex: MatchError => 
				none(s"Regression.train error ${ex.getMessage()} caused by ${ex.getCause}" )
			case _ => none("Regression error", e)
		}
	}
}


	/**
	 * Companion object that define different versions of the dot product of
	 * data (array of values) and weights.
	 * @author Patrick Nicolas
	 * @version 0.99.1
	 */
object Regression {
	
	final def dot[T <: AnyVal](x: Array[T], w: DblArray)(implicit f: T => Double): Double = 
		x.zip(w.drop(1)).map { case (_x, _w) => _x*_w}.sum + w.head
		
	final def dot[T <: AnyVal](x: Vector[T], w: DblVector)(implicit f: T => Double): Double = 
		x.zip(w.drop(1)).map { case (_x, _w) => _x*_w}.sum + w.head
	  	
	
	final def dot[T <: AnyVal](x: Array[T], m: RegressionModel)(implicit f: T => Double): Double = 
		x.zip(m.weights.drop(1)).map { case (_x, w) => _x*w}.sum + m.weights.head
}

// ----------------------------  EOF -----------------------