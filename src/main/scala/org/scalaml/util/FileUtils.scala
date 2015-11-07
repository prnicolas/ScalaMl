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
 * Version 0.99
 */
package org.scalaml.util

import scala.io.Source._
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.LoggingUtils._

		/**
		 * Basic utility singleton to read and write content from and to a file
		 * @author Patrick Nicolas
		 * @since December 2, 2013
		 * @note Scala for Machine Learning
		 */
object FileUtils {
	private val logger = Logger.getLogger("FileUtils")

		/**
		 * Read the content of a file as a String
		 * @param pathName Name of the file to read the content form
		 * @param className Name of the class to read from 
		 * @return Content of the file if successful, None otherwise
		 */
	def read(toFile: String, className: String): Option[String] = 
			Try(fromFile(toFile).mkString).toOption

		/**
		 * Write the content into a file. The content is defined as a string.
		 * @param conntent content to write into a file
		 * @param pathName Name of the file to read the content form
		 * @param className Name of the class to read from 
		 * @return true is successful, false otherwise
		 */
	def write(content: String, pathName: String, className: String): Boolean = {
		import java.io.PrintWriter
	
		var printWriter: Option[PrintWriter] = None
		var status = false
		Try {
			printWriter = Some(new PrintWriter(pathName))
			printWriter.map( _.write(content) )
			status = true
		} 
		match {
				// Catch and display exception description and return false
			case Failure(e) => {
				DisplayUtils.error(s"$className.write failed for $pathName", logger, e)
				
				if( printWriter != None) {
					Try(printWriter.map(_.close) ) match {
						case Success(res) => res
						case Failure(e) => 
								DisplayUtils.error(s"$className.write Failed for $pathName", logger, e)
					}
				}
			}
			case Success(s) => { }
		}
		status
	}
}

// ---------------------------------  EOF -------------------------------------