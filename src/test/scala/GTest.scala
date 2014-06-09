


object GTest extends App {
   val series = Array[Double](1.9, 1., 4.0, 5.9, 11)
   val r: Range = 0 until 5
   r filter( _ != 3) foreach( println )
   
}


// ------------------------  EOF ----------------------------------------