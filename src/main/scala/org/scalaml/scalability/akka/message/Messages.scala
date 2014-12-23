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
package org.scalaml.scalability.akka.message


import akka.actor._
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.XTSeries


		/**
		 * <p>Generic message exchanged between a master and worker actors.</p>
		 * @param id  Unique identifier for this message.
		 * @author Patrick Nicolas
		 * @since March 28, 2014
		 * @note Scala for Machine learning Chapter 12 Scalable Framework / Akka / Master-workers
		 */
sealed abstract class Message(val id: Int)

		/**
		 * <p>Message sent by the master to terminate the worker actors..</p>
		 * @param i unique identifier for this message.
		 * @author Patrick Nicolas
		 * @since March 28, 2014
		 * @note Scala for Machine learning Chapter 12 Scalable Framework / Akka / Master-workers
		 */
case class Terminate(i: Int) extends Message(i)

		/**
		 * <p>Message sent to the master to initialize the computation.</p>
		 * @param i unique identifier for this message.
		 * @author Patrick Nicolas
		 * @since March 28, 2014
		 * @note Scala for Machine learning Chapter 12 Scalable Framework / Akka / Master-workers
		 */
case class Start(i: Int =0) extends Message(i)

		/**
		 * <p>Message sent by the worker actors to notify the master their tasks is completed.</p>
		 * @param i unique identifier for this message.
		 * @param xt time series transformed (or processed)
		 * @author Patrick Nicolas
		 * @since March 28, 2014
		 * @note Scala for Machine learning Chapter 12 Scalable Frameworks / Akka / Master-workers
		 */
case class Completed(i: Int, xt: XTSeries[Double]) extends Message(i)

		/**
		 * <p>Message sent by the master to the worker actors to start the computation.</p>
		 * @param id unique identifier for this message.
		 * @param xt time series to transform (or process)
		 * @author Patrick Nicolas
		 * @since March 28, 2014
		 * @note Scala for Machine learning Chapter 12 Scalable Frameworks / Akka / Master-workers
		 */
case class Activate(i: Int, xt: XTSeries[Double]) extends Message(i)



// ---------------------------------  EOF -------------------------