/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap12


import akka.actor._
import org.scalaml.core.Types.ScalaMl._

sealed abstract class Message(val id: Int)
case class Terminate(val _id: Int) extends Message(_id)

case class Start(val _id: Int =0) extends Message(_id)

case class Completed(val _id: Int, val variance: Double) extends Message(_id)

case class Activate(val _id: Int, 
		            val data: Array[(XY, Int)], 
		            val sender: ActorRef) extends Message(_id)



// ---------------------------------  EOF -------------------------