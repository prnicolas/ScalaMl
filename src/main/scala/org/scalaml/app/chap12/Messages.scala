package org.scalaml.app.chap12

import scala.collection.parallel.mutable.ParArray
import scala.collection.mutable.ArraySeq
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.actors.Actor
import scala.collection.mutable.ArrayBuffer



sealed abstract class Message(val id: Int)
case class Terminate(val _id: Int) extends Message(_id)
case class Start(val _id: Int =0) extends Message(_id)
case class Completed(val _id: Int, w: Array[Double]) extends Message(_id)
case class Activate(val _id: Int, 
		            val data: Array[(Double, Double)], 
		            val weights: (Double, Double), 
		            val sender: Actor) extends Message(_id)
case class Iterate(val _id: Int, val weights: (Double, Double), val sender: Actor) extends Message(_id)





// ---------------------------------  EOF -------------------------