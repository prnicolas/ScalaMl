/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.workflow.data


import org.scalaml.core.XTSeries
import org.scalaml.workflow.PipeOperator
import java.io.{FileNotFoundException, IOException, PrintWriter}


		/**
		 * Generic class to load or save files into either HDFS or local files system
		 */
   
class DataSink[T <% String](val path: String) extends PipeOperator[List[XTSeries[T]], Int] {
   import XTSeries._
   import scala.io.Source
   
   final val CSV_DELIM = ","
   type TxtFields = Array[String]
    
   def write(content: String): Boolean = {
     import java.io.{PrintWriter, IOException, FileNotFoundException}
     
     var printWriter: PrintWriter = null
     try {
	     printWriter = new PrintWriter(path)
	     printWriter.write(content)
	     true
     }
     catch {
       case e: IOException => Console.println(e.toString); false
       case e: FileNotFoundException => Console.println(e.toString); false
     }
     finally {
        if( printWriter != null) 
          try {
            printWriter.close
          }
          catch {
           case e: IOException => Console.println(e.toString)
           case e: FileNotFoundException => Console.println(e.toString)
         }
      }
   }
   
    
   override def |> (xs: List[XTSeries[T]]): Option[Int] = {
     import java.io.{PrintWriter, IOException, FileNotFoundException}
     
     var printWriter: PrintWriter = null
     try {
         val content = new StringBuilder
         val numValues = xs(0).size-1
         val last = xs.size-1
         var k = 0
         while( k < numValues) {
        	val values = xs.toArray
            (0 until last) foreach(j => content.append(values(j)(k)).append(CSV_DELIM) )
            content.append(values(last)(k)).append("\n")
            k += 1
         }
   
	     printWriter = new PrintWriter(path)
	     printWriter.write(content.toString)
	     Some(k)
     }
     catch {
       case e: IOException => Console.println(e.toString); None
       case e: FileNotFoundException => Console.println(e.toString); None
     }
     finally {
        if( printWriter != null) 
          try {
            printWriter.close
          }
          catch {
           case e: IOException => Console.println(e.toString)
           case e: FileNotFoundException => Console.println(e.toString)
         }
      }
   }
}


object DataSink {
  import scala.annotation.implicitNotFound
  
  @implicitNotFound("Conversion of paramerized type to String for DataSink undefined")
  def apply[T](path: String)(implicit f: T => String= (t:T)=>t.toString): DataSink[T] = new DataSink[T](path)
}

// ----------------------------------   EOF ----------------------------------------------