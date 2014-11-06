/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
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
	   case Success(status) => Display.show(status, logger)
	   case Failure(e) => Display.error(args(0), logger, e)
     }
  }
  
  protected def execute(args: Array[String]): String
}


// --------------------------  EOF -------------------------------