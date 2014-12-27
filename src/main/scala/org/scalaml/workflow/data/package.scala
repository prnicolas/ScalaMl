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
package org.scalaml.workflow

		/**
		 * This package object that defines the class to manage extraction of data or documents
		 * from file as well as generation of data into file(s):<br>
		 * - Source of data (file(s), or directory() using CSV format <b>DataSource</b><br>
		 * - Source for extraction and management of documents from a Corpus <b>DocumentsSource</b><br>
		 * - Sink of data to be store in file(s) <b>DataSink</b><br>
		 * The sources and sink classes are implemented as data transform (PipeOperator)
		 *  @note Scala for Machine Learning - Appendix / Scala programming 
		 */
package object data { }
// ---------------------------------------  EOF -----------------------------------------