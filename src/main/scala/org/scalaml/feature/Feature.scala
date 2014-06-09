package org.scalaml.feature

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag



final class Feature[T <% Double](val values: Array[T]) (implicit conv: Double => T, m: ClassTag[T])  {
    require( values != null && values.size > 1, "Feature has incorrect dimension")
	
	def + (f: Feature[T]) : Feature[T] = {
       new Feature[T](applyOp(f, (x: Double, y: Double) => x+y))
	}
    
    def - (f: Feature[T]): Feature[T] = {
       new Feature[T](applyOp(f, (x: Double, y: Double) => x-y))
	}
    
    def * (alpha: T) : Feature[T] = {
    	new Feature[T] (values.foldLeft(new ArrayBuffer[T])((sBuf, v) => {sBuf.append(v*alpha); sBuf} ).toArray)
    }
    
    def dot (f: Feature[T]) : Feature[T] = {
      new Feature[T](applyOp(f, (x: Double, y: Double) => x*y))
    }
    
    private[this] def applyOp(f: Feature[T], op: (Double, Double)=>Double): Array[T] = {
    	var i = -1
    	values.foldLeft(new ArrayBuffer[T])((sBuf, v) => {i += 1; sBuf.append(op(v, f.values(i))); sBuf} ).toArray
    }
}

//----------------------------------------  EOF -----------------------------------------------------------
    




// -----------------------  EOF -------------------------------------