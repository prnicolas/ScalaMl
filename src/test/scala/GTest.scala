


import org.apache.log4j.Logger

trait Operator[T, U] {
	def |> : PartialFunction[T, Option[U]]
}

class MyOperator extends Operator[Int, Int] {
override def |> : PartialFunction[Int, Option[Int]] = {
	 case n: Int if( n > 0) => Some(n<<2)
	}
}

trait A {
  val x: Double
}

class B extends A {
  val x: Double = 6
}

object GTestApp extends App {

   def pow(x: Double, y: Double): Double = Math.pow(x,y)
   val sqrt = pow(_: Double, 0.5)
   println(sqrt(3))
   
   def _sqrt: PartialFunction[Double, Double] = {
     case x: Double if(x >= 0.0) => sqrt(x)
   }

   /*
   val myOperator = new MyOperator 
   val y: Int = -2
   println( (myOperator.|>(y)).get )
   * 
   */
   
}


// ------------------------  EOF ----------------------------------------