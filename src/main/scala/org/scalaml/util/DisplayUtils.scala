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
package org.scalaml.util

import org.apache.log4j.Logger
import org.scalaml.core.Types

	/**
	 * <p>Singleton wrapper for information or debugging information.</p>
	 *  @author Patrick Nicolas
	 *  @since December 3, 2013
	 *  @note Scala for Machine Learning
	 */
object DisplayUtils {	
	private val DEFAULT_SHOW_RETURN = 0			// Default return value after info display
	private val DEFAULT_ERROR_RETURN = -1		// Default return value for error
	private val DEST_CONSOLE = 0x01					// Flag to dump computation results on std out
	private val DEST_LOGGER = 0x02					// Flag to dump computation results into log4j log
	private val DEST_CHART = 0x04						// Flag to plot computation results
	
	private val LOG_DESTINATION = Map[String, Int](
		"console" -> 0x01, "logger" -> 0x02, "chart" -> 0x04, "none" -> 0x00
	)
	
	private var destination: Int = DEST_CONSOLE + DEST_CHART
	
		/**
		 * Initialize the display configuration using the argument passed in the command line 
		 * <b>sbt "test:run options</b>/
		 * @param args command line arguments.
		 */
	def init(args: Array[String]): Unit  = 
		destination = args.foldLeft(0)((dest, arg) => 
				dest + LOG_DESTINATION.getOrElse(arg, 0))
			
		/**
		 * Test if plotting of computation results has been enabled
		 * @return true if charts have to be displayed, false otherwise
		 */
	final def isChart: Boolean = ((destination & 0x04) == 0x04)

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
		else {
			val blankChars = new Array[Char](length - label.length)
			label + new String(blankChars)
		}
	}
  
		/**
		 * Display the value of parameterized type in either standard output,
		 * or log4j log or both
		 * @param t value to be displayed
		 * @param logger Reference to the log4j log appender
		 * @param alignement Align the label within its placement if alignment is greater than 
		 * the label, no alignment otherwise
		 * @return 0
		 */
	final def show[T](t: T, logger: Logger, alignment: Int = -1): Int = { 
		print(if(alignment != -1) align(t.toString, alignment) else t.toString, logger)
		DEFAULT_SHOW_RETURN
	}
	
			/**
		 * Display the sequence of parameterized type in either standard output,
		 * or log4j log or both
		 * @param t value to be displayed
		 * @param logger Reference to the log4j log appender
		 * @return 0
		 */
	final def show[T](seq: Seq[T], logger: Logger): Int = {
		seq.mkString(" ")
		DEFAULT_SHOW_RETURN
	}
 
		/**
		 * Display the error related to the value of a parameterized
		 * @param t value to be displayed
		 * @param logger Reference to the log4j log appender
		 * @return -1
		 */
	final def error[T](t: T, logger: Logger): Int = {
		processError(t, logger)
		DEFAULT_ERROR_RETURN
	}

		/**
		 * Display the content of an exception related to the value of a parameterized.
		 * The stack trace related to the exception is printed
		 * @param t value to be displayed
		 * @param logger Reference to the log4j log appender
		 * @param e Exception caught
		 * @return -1
		 */
	final def error[T](t: T, logger: Logger, e: Throwable): Int = {
		processError(t, logger, e)
		DEFAULT_ERROR_RETURN
	}
  
		/**
		 * Display the error related to the value of a parameterized.
		 * @param t value to be displayed
		 * @param logger Reference to the log4j log appender
		 * @return Mpme
		 */
	final def none[T](t: T, logger: Logger): None.type = none(t, logger)

		/**
		 * Display the content of an exception related to the value of a parameterized.
		 * The stack trace related to the exception is printed
		 * @param t value to be displayed
		 * @param logger Reference to the log4j log appender
		 * @param e Exception caught
		 * @return None
		 */
	final def none[T](t: T, logger: Logger, e: Throwable): None.type = {
		processError(t, logger, e)
		None
	}
	
	private def print(msg: String, logger: Logger): Unit = {
		if( (destination & 0x01) == 0x01)
			Console.println(msg)
		if( (destination & 0x02) == 0x02)
			{logger.error(msg); println("log") }
	}

	private def processError[T](t: T, logger: Logger, e: Throwable): Unit = 
		print(s"Error: ${t.toString} with ${e.toString}", logger)

	private def processError[T](t: T, logger: Logger): Unit = 
	  print(s"Error: ${t.toString}", logger)
}


// -----------------------------  EOF --------------------------------