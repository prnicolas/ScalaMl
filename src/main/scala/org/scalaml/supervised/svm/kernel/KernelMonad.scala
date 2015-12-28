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
 * Version 0.99
 */
package org.scalaml.supervised.svm.kernel

import scala.language.implicitConversions

import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.core.functional._Monad

object KernelMonad {
  
	type F1 = Double => Double
	type F2 = (Double, Double) => Double
	
	case class KF[G](val g: G, h: F2) {
		def metric(v: DblVector, w: DblVector)(implicit gf: G => F1): Double =
			g(v.zip(w).map{ case(_v, _w) => h(_v, _w)}.sum)
	}
	
	implicit def hg2KF[G](hg: (G, F2)): KF[G] = KF(hg._1, hg._2)
	
	val identity = (x: Double, y: Double) => x*y
	
	val kfMonad = new _Monad[KF] {
		override def unit[G](g: G): KF[G] = KF[G](g, identity)
		override def map[G, H](kf: KF[G])(f: G => H): KF[H] = KF[H](f(kf.g), kf.h)
		override def flatMap[G, H](kf: KF[G])(f: G => KF[H]): KF[H] =
			KF[H](f(kf.g).g, kf.h)
	}
	
	implicit class kF2Monad[G](kf: KF[G]) {
		def map[H](f: G => H): KF[H] = kfMonad.map(kf)(f)
		def flatMap[H](f: G => KF[H]): KF[H] = kfMonad.flatMap(kf)(f)
	}
	

	class RBF(s2: Double) extends KF[F1]((x: Double) => Math.exp(-0.5*x*x/s2), (x: Double, y: Double) => x -y)
	class Polynomial(d: Int) extends KF[F1]((x: Double) => Math.pow(1.0+x, d), (x: Double, y: Double) => x*y)
	

}


object KernelMonadApp extends {
	import KernelMonad._

	val v = Vector[Double](0.5, 0.2, 0.3)
	val w = Vector[Double](0.1, 0.7, 0.2)
	val composed = for {
		kf1 <- new RBF(0.6)
		kf2 <- new Polynomial(6)
	} yield kf2

	composed.metric(v, w)
}

// -------------------------------  EOF ------------------------------------