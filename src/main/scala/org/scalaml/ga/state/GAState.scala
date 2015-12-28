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
package org.scalaml.ga.state


		/**
		 * Class that defines the state of the genetic algorithm
		 * based solver (or optimizer). The state is used to control the execution
		 * of the replication process.
		 * @constructor Create a specific state for the execution of the genetic algorithm
		 * @param description  Description of the state
		 * @author Patrick Nicolas
		 * @since August 13, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
sealed abstract class GAState(description: String)

		/**
		 * Define the SUCCESS state for the genetic algorithm-based solver
		 * '''description:'''   Description of the state
		 * @constructor Create a specific state for the execution of the genetic algorithm
		 * @param description description of the state
		 * @author Patrick Nicolas
		 * @since August 13, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
case class GA_SUCCEED(description: String) extends GAState(description)


		/**
		 * Define the FAILED state for the genetic algorithm-based solver
		 * '''description:''' Description of the state>
		 * @constructor Create a FAILED state for the execution of the genetic algorithm
		 * @param description description of the state
		 * @author Patrick Nicolas
		 * @since August 13, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
case class GA_FAILED(description: String) extends GAState(description)

		/**
		 * Define the RUNNING state for the genetic algorithm-based solver.
		 * @constructor Create a FAILED state for the execution of the genetic algorithm
		 * @author Patrick Nicolas
		 * @since August 13, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
object GA_RUNNING extends GAState("Running")

		/**
		 * Define the NOT RUNNING state for the genetic algorithm-based solver
		 * @constructor Create a FAILED state for the execution of the genetic algorithm
		 * @author Patrick Nicolas
		 * @since August 13, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
object GA_NOT_RUNNING extends GAState("Not Running")

		/**
		 * Define the NON CONVERGENCE state for the genetic algorithm-based solver
		 * '''description:''' Description of the state>/p>
		 * @constructor Create a NON CONVERGENCE state for the execution of the genetic algorithm
		 * @param description description of this state
		 * @author Patrick Nicolas
		 * @since August 13, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
case class GA_NO_CONVERGENCE(description: String) extends GAState(description)


// ----------------------------  EOF ------------------------------