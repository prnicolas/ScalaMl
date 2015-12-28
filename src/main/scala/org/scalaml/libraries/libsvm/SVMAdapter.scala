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
package org.scalaml.libraries.libsvm


import scala.collection.mutable.ArrayBuffer
import libsvm.{svm_problem, svm_node, svm, svm_model, svm_parameter}
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.supervised.svm.SVMModel



object SVMAdapter {
 
	type SVMNodes = Array[Array[svm_node]]
  	
	class SVMProblem(numObs: Int, labels: DblArray) {
		val problem = new svm_problem
		problem.l = numObs
		problem.y = labels 
		problem.x = new SVMNodes(numObs)
		
		def update(n: Int, node: Array[svm_node]): Unit = 
			problem.x(n) = node
	}
	
	def createNode(dim: Int, x: DblArray): Array[svm_node] = {
		val newNode = new Array[svm_node](dim)
		x.zipWithIndex.foreach{ case (y, j) =>
			val node = new svm_node
			node.index= j
			node.value = y
			newNode(j) = node 
		}
		newNode
	}
  
	def predictSVM(model: SVMModel, x: DblArray): Double =
		svm.svm_predict(model.svmmodel, toNodes(x))
		
		
	def crossValidateSVM(
			problem: SVMProblem, 
			param: svm_parameter, 
			nFolds: Int,
			size: Int): DblArray = {
	  
	  val target = Array.fill(size)(0.0)
		svm.svm_cross_validation(problem.problem, param, nFolds, target)
		target
	}

	def trainSVM(problem: SVMProblem, param: svm_parameter): svm_model =
			svm.svm_train(problem.problem, param)
			
	
	private def toNodes(x: DblArray): Array[svm_node] = 
		x.view.zipWithIndex./:(new ArrayBuffer[svm_node])((xs, f) =>  {
			val node = new svm_node
			node.index = f._2
			node.value = f._1
			xs.append(node)
			xs
		}).toArray
}

// ------------------------------  EOF --------------------