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
package org.scalaml.supervised

		/**
		 * This package object contains the classes implementing the conditional random fields
		 * for modeling sequential data using the Indian Institute of Technology, Bombay (IITB):<br>
		 * - Iterator to generate sequential data for the condition random field <b>CrfSeqIter</b><br>
		 * - Configuration of the conditional random fields with L2 regularization <b>CrfConfig</b><br>
		 * - Definition of data sequence used in training of Conditional Random fields 
		 * <b>CrfTrainingSet</b><br>
		 * - Implementation of the Conditional Random fields using the iitb CRF library <b>Crf</b><br>
		 * - Definition of a CRF model <b>CrfModel</b>
		 * @see iitb CRF library http://sourceforge.net/projects/crf/
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Conditional random fields
		 */
package object crf { }
// ---------------------------------------  EOF -----------------------------------------