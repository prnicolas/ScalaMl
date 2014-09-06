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
package org.scalaml.util

import org.apache.log4j.Logger

	/**
	 * <p>Singleton wrapper for information or debugging information.</p>
	 *  @author Patrick Nicolas
	 *  @since December 3,  2013
	 *  @note Scala for Machine Learning
	 */
object Display {	
		// Dump information message onto standard output or logger
  final def show[T](t: T): Unit = println(t.toString)
  final def show[T](t: T, logger: Logger): Unit = logger.info(t.toString)
  final def show[T](seq: Seq[T], logger: Logger): Unit = logger.info(seq.foreach(t => Display.show(t, logger)))
  
  		// Dump debugging message and exception in standard output or logger
  final def error[T](t: T): Unit = println("Error: " + t.toString)
  final def error[T](t: T, e: Throwable): Unit =  println("Error: " + t.toString + e.toString)
  final def error[T](t: T, logger: Logger): Unit = logger.debug(t.toString)
  final def error[T](t: T, logger: Logger, e: Throwable): Unit = logger.debug(t.toString + " - " + e.toString)

}


// -----------------------------  EOF --------------------------------