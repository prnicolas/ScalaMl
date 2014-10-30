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
package org.scalaml.util

import org.apache.log4j.Logger

	/**
	 * <p>Singleton wrapper for information or debugging information.</p>
	 *  @author Patrick Nicolas
	 *  @since December 3,  2013
	 *  @note Scala for Machine Learning
	 */
object Display {	
  val loggerFlag = false
  
		// Dump information message onto standard output or logger
  
  final def align(label: String, length: Int): String = {
  	 if( length < label.size)
  		label
  	 else 
  		label + Range(label.size, length).foldLeft(new StringBuilder)((b, n) =>b.append(" "))
  }
  
  final def show[T](t: T, logger: Logger, alignment: Int = -1) = { 
  	 val text = if(alignment != -1) align(t.toString, alignment) else t.toString
     if(loggerFlag) logger.info(text)
     else Console.println(text)
     0
  }
  final def show[T](seq: Seq[T], logger: Logger): Int = {
    val info = seq.foldLeft(new StringBuilder)((buf, el) => buf.append(s"$el ")).toString
    if( loggerFlag) logger.info(info)
    else Console.println(seq.foldLeft(new StringBuilder)((buf, el) => buf.append(s"$el ")).toString)
    0
  }
 
  		// Dump debugging message and exception in standard output or logger
  final def error[T](t: T): Int = { Console.println(s"Error: ${t.toString}"); -1}
  final def error[T](t: T, e: Throwable): Int =  { 
      Console.println(s"Error: ${t.toString} with ${e.toString}")
      e.printStackTrace
      -1
  }
  final def error[T](t: T, logger: Logger): Int = { 
     if(loggerFlag) logger.error(s"Error: ${t.toString}")
     else error(t)
     -1
  }
  final def error[T](t: T, logger: Logger, e: Throwable): Int = { 
  	 if(loggerFlag) logger.error(s"Error: ${t.toString} with ${e.toString}")
  	 else error(t,e)
  	 e.printStackTrace
  	 -1 
  }
}


// -----------------------------  EOF --------------------------------