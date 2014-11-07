/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
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
 

  final def error[T](t: T, logger: Logger): Int = error(t, logger, null)

  final def error[T](t: T, logger: Logger, e: Throwable): Int = {
     processError(t, logger, e)
  	 -1 
  }
  
  final def none[T](t: T, logger: Logger): None.type = none(t, logger, null)

  final def none[T](t: T, logger: Logger, e: Throwable): None.type = {
     processError(t, logger, e)
  	 None
  }
  private def processError[T](t: T, logger: Logger, e: Throwable): Unit = {
      val msg = if(e != null) s"Error: ${t.toString} with ${e.toString}" else s"Error: ${t.toString}"
      if(loggerFlag) logger.error(msg)
      else Console.println(msg)
      if( e != null) e.printStackTrace
  }
}


// -----------------------------  EOF --------------------------------