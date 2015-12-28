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
package org.scalaml.supervised

		/**
		 * This package object contains the classes implementing the conditional random fields
		 * for modeling sequential data using the Indian Institute of Technology, Bombay (IITB):<br>
		 * - Iterator to generate sequential data for the condition random field '''CrfSeqIter'''<br>
		 * - Configuration of the conditional random fields with L2 regularization '''CrfConfig'''<br>
		 * - Definition of data sequence used in training of Conditional Random fields 
		 * '''CrfTrainingSet'''<br>
		 * - Implementation of the Conditional Random fields using the IITB CRF library '''Crf'''<br>
		 * - Definition of a CRF model '''CrfModel'''
		 * @see iitb CRF library http://sourceforge.net/projects/crf/
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Conditional random fields
		 */
package object crf { }
// ---------------------------------------  EOF -----------------------------------------