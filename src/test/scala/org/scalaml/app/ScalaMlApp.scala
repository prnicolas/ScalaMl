/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app

import org.apache.log4j.Logger
import scala.util.{Try, Success, Failure}
import org.scalaml.util.Display


	/**
	 * 
	 */
trait ScalaMlApp {
  private val logger = Logger.getLogger("ScalaMlApp")
  
  def process(args: Array[String]): Unit = {
    Try(execute(args)) match {
	   case Success(status) => Display.show(status)
	   case Failure(e) => Display.error(args(0), e)
     }
  }
  
  protected def execute(args: Array[String]): String
}


// --------------------------  EOF -------------------------------