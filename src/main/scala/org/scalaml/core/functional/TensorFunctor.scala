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



object TensorFunctor {

    /**
     * Definition of the Functor for the the vector field. The transformation is implemented
     * through a covariant functor 'map'
     */
  
  type Hom[T] = {
    type Right[X] = (X) => T
    type Left[X] = (T) => X
  }
  
  
  trait VectorFtor[T] extends Functor[(Hom[T])#Left] { 
    self =>
      override def map[U,V](vu: Function1[T,U])(f: U =>V): 
        Function1[T,V] = f.compose(vu)
  }

  
     /**
      * Definition of the type of co-variant vector fields a Vector Field => Field
     */

  
     /**
      * Definition of the contra-variant functor for the co-vector fields. The
      * implementation relies on the Lambda type projection on co-vector field (type _VField)
      */
  
  trait CoVectorFtor[T] extends CoFunctor[(Hom[T])#Right] {
    self =>
      override def map[U,V](vu: (U) => T)(f: V =>U):
         (V) => T = f.andThen(vu)
  }
  
  implicit class coVector2Ftor[U, T](vu: (U) => T) extends CoVectorFtor[T] {
    final def map[V](f: V => U): (V) => T = super.map(vu)(f)
        
    def compose[V, W](f: V => U, g: W => V): (W) => T = super.map(vu)(f).map(g)
  }
}


// -------------------------  EOF ------------------------------------