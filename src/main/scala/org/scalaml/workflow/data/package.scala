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
package org.scalaml.workflow

		/**
		 * This package object that defines the class to manage extraction of data or documents
		 * from file as well as generation of data into file(s)
		 * 
		 * - Source of data (file(s), or directory() using CSV format ''DataSource''
		 * - Source for extraction and management of documents from a Corpus ''DocumentsSource''
		 * - Sink of data to be store in file(s) ''DataSink''
		 * The sources and sink classes are implemented as data transform (PipeOperator)
		 * @see Scala for Machine Learning - Appendix / Scala programming 
		 */
package object data { }

// ---------------------------------------  EOF -----------------------------------------