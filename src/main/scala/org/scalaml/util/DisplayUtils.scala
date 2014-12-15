/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.3
 */
package org.scalaml.util

import org.apache.log4j.Logger
import org.scalaml.core.Types

	/**
	 * <p>Singleton wrapper for information or debugging information.</p>
	 *  @author Patrick Nicolas
	 *  @since December 3,  2013
	 *  @note Scala for Machine Learning
	 */
object DisplayUtils {	
	private val DEFAULT_SHOW_RETURN = 0
	private val DEFAULT_ERROR_RETURN = -1
	val loggerFlag = false
  
		/**
		 * Global function that align a label against a boundary. There is no alignment if the 
		 * size of the placement is smaller than the actual label.
		 * @param label Label to be aligned
		 * @param length Length of the box the label has to be aligned
		 */
	final def align(label: String, length: Int): String = {
		require(label != Types.nullString, "DisplayUtils.align Label is undefined")
		require(length < 128, 
				s"DisplayUtils.align Size of label placement ${label.length} is incorrect")
		
		if( length < label.length)
			label
		else 
			label + Range(label.length, length).foldLeft(new StringBuilder)((b, n) =>b.append(" "))
	}
  
		/**
		 * DisplayUtils the value of parameterized type in either standard output,
		 * or log4j log or both
		 * @param t value to be displayed
		 * @param logger Reference to the log4j log appender
		 * @param alignement Align the label within its placement if alignment is greater than 
		 * the label, no alignment otherwise
		 */
	final def show[T](t: T, logger: Logger, alignment: Int = -1): Int = { 
		val text = if(alignment != -1) align(t.toString, alignment) else t.toString
		if(loggerFlag) 
			logger.info(text)
		else 
			Console.println(text)
		DEFAULT_SHOW_RETURN
	}
	
	
	final def show[T](seq: Seq[T], logger: Logger): Int = {
		val info = seq.foldLeft(new StringBuilder)((buf, el) => buf.append(s"$el ")).toString
		if( loggerFlag) 
			logger.info(info)
		else 
			Console.println(seq.foldLeft(new StringBuilder)((buf, el) => buf.append(s"$el ")).toString)
		DEFAULT_SHOW_RETURN
	}
 

	final def error[T](t: T, logger: Logger): Int = error(t, logger)

	final def error[T](t: T, logger: Logger, e: Throwable): Int = {
		processError(t, logger, e)
		DEFAULT_ERROR_RETURN
	}
  

	final def none[T](t: T, logger: Logger): None.type = none(t, logger)


	final def none[T](t: T, logger: Logger, e: Throwable): None.type = {
		processError(t, logger, e)
		None
	}
	
	private def processError[T](t: T, logger: Logger, e: Throwable): Unit = {
		val msg = s"Error: ${t.toString} with ${e.toString}" 
		
		if(loggerFlag) logger.error(msg) else Console.println(msg)
		e.printStackTrace
	}
	
		private def processError[T](t: T, logger: Logger): Unit = {		
			if(loggerFlag) 
				logger.error(s"Error: ${t.toString}") 
			else 
				Console.println(s"Error: ${t.toString}")
	}
}


// -----------------------------  EOF --------------------------------