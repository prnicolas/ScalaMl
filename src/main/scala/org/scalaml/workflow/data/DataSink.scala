/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.workflow.data


import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.workflow.PipeOperator
import java.io.{FileNotFoundException, IOException, PrintWriter}
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display


		/**
		 * Generic class to load or save files into either HDFS or local files system
		 */
   
class DataSink[T <% String](val path: String) extends PipeOperator[List[XTSeries[T]], Int] {
   import XTSeries._
   import scala.io.Source
   
   private val logger = Logger.getLogger("DataSink")
   final val CSV_DELIM = ","
   type TxtFields = Array[String]
    
   def write(content: String): Boolean = {
     import java.io.{PrintWriter, IOException, FileNotFoundException}
     
     var printWriter: PrintWriter = null
     Try {
	     printWriter = new PrintWriter(path)
	     printWriter.write(content)
	     true
     } match {
    	 case Success(res) => res
    	 case Failure(e) => {
    		 Display.error("DataSink.write ", logger, e)
    		 
    		 if( printWriter != null) {
	             Try { printWriter.close; false }
	             match {
	    		    case Success(res) => false
	    	        case Failure(e) =>  Display.error("DataSink.write ", logger, e); false
	    		 }
    		 }
    		 else false
          }
     }
   }

   
   def |> (v: DblVector): Option[Int] = {
  	 import java.io.{PrintWriter, IOException, FileNotFoundException}
     
     var printWriter: PrintWriter = null
     Try {
    	 val content = v.foldLeft(new StringBuilder)((b, x) => b.append(x).append(CSV_DELIM))
    	 content.setCharAt(content.size-1, ' ')
    	 write(content.toString.trim)
    	 1
     } match {
    	 case Success(res) => Some(res)
    	 case Failure(e) => {
    		 Display.error("DataSink.|> ", logger, e)
    		  
    		 if( printWriter != null) {
    		    Try {printWriter.close; 0 }
    		    match {
    		    	case Success(res) => Some(res)
    		    	case Failure(e) => Display.error("DataSink.|> ", logger, e); None
    		    }
    		  }
    		 else None
    	 }
     }
   }
    
   override def |> (xs: List[XTSeries[T]]): Option[Int] = {
     import java.io.{PrintWriter, IOException, FileNotFoundException}
     
     var printWriter: PrintWriter = null
     Try {
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
	     k
     } match {
    	 case Success(k) => Some(k)
    	 case Failure(e) => {
    		  Display.error("DataSink.|> ", logger, e)
    		  
    		 if( printWriter != null) {
    		    Try {printWriter.close; 0 }
    		    match {
    		    	case Success(res) => Some(res)
    		    	case Failure(e) => Display.error("DataSink.|> ", logger, e); None
    		    }
    		  }
    		 else None
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