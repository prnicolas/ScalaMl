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
 * Version 0.99.1
 */
package org.scalaml.core.functional

import scala.language.higherKinds

  /**
   * Generic Covariant functor
   * @tparam M[_] Single type Higher hind type 
   * @author Patrick Nicolas
   * @version 0.99.1.1
   * @see Scala for Machine Learning Chapter 1 "Getting Started" Abstraction / 
   * Covariant functors
   */
 trait Functor[M[_]] {
		/**
		 * Declare the signature of the covariant functor.
		 * @tparam U type of input to the morphism
		 * @tparam V type of the output from the morphism
		 * @param m instance of a class of type M[U]
		 * @param f morphism
		 * @return instance of the class parameterized by the output type V
		 */
	def map[U,V](m: M[U])(f: U => V): M[V]
 }


		/**	
		 * Generic Contravariant functor
		 * @tparam M[_] Single type Higher hind type 
		 * @author Patrick Nicolas
		 * @version 0.99.1.1
		 * @see Scala for Machine Learning Chapter 1 "Getting Started" Abstraction / 
		 * Contravariant functors
		 */

trait CoFunctor[M[_]] {
		/**
		 * Declare the signature of the contravariant functor.
		 * @tparam U type of input to the morphism
		 * @tparam V type of the output from the morphism
		 * @param m instance of a class of type M[U]
		 * @param f morphism V => U
		 * @return instance of the class parameterized by the output type V
		 */
	def map[U,V](m: M[U])(f: V =>U): M[V]
}
 
 
		/**
		 * Generic BiFunctor 
		 * @tparam M[_, _] Two type Higher hind type 
		 * @author Patrick Nicolas
		 * @version 0.99.1.1
		 */
trait BiFunctor[M[_, _]] {
		/**
		 * Declare the signature of the bi-functor.
		 * @tparam U type of input to the first morphism
		 * @tparam W type of the output from the first morphism
		 * @tparam V type of input to the second morphism
		 * @tparam Z type of the output from the second morphism
		 * @param m instance of a class of type M[U, V]
		 * @param fuv morphism U => W
		 * @param fvz morphism V => Z
		 * @return instance of the class parameterized by the output type [W,Z]
		 */
	def map[U, V, W, Z](m: M[U,V])(fuv: U => W, fvz: V=> Z): M[W, Z]
}
 
 
		/**
		 * Generic '''cartesian product''' of a contravariant and covariant functors
		 * @tparam M[_] Single type Higher hind representing the contravariant functor
		 * @tparam N[_] Single type Higher hind representing the covariant functor
		 * @author Patrick Nicolas
		 * @version 0.99.1.1
		 */
trait BiProdFunctor[M[_, _], N[_,_]]  
		extends BiFunctor[({type L[X, Y] = (M[X, Y], N[X, Y])})#L ] {
		// abstract values
	val bf: BiFunctor[M]
			val bg: BiFunctor[N]
	
	
	override def map[U, V, W, Z]
			(mnuv: (M[U, V], N[U,V]))
			(fuw: U => W, fvz: V => Z): (M[W, Z], N[W,Z]) = 
		(bf.map(mnuv._1)(fuw, fvz), bg.map(mnuv._2)(fuw, fvz))
} 



// -------------------------  EOF ------------------------------------