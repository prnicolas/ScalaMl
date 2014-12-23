/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.ga.state


		/**
		 * <p>Class that defines the state of the genetic algorithm
		 * based solver (or optimizer). The state is used to control the execution
		 * of the replication process.</p>
		 * @constructor Create a specific state for the execution of the genetic algorithm
		 * @param description  Description of the state
		 * @author Patrick Nicolas
		 * @since August 13, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
sealed abstract class GAState(val description: String)

		/**
		 * <p>Define the SUCCESS state for the genetic algorithm-based solver.<br>
		 * <b>description:</b>   Description of the state>/p>
		 * @constructor Create a specific state for the execution of the genetic algorithm
		 * @param description description of the state
		 * @author Patrick Nicolas
		 * @since August 13, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
case class GA_SUCCEED(val _description: String) extends GAState(_description)


		/**
		 * <p>Define the FAILED state for the genetic algorithm-based solver<br>
		 * <b>description:</b> Description of the state>/p>
		 * @constructor Create a FAILED state for the execution of the genetic algorithm
		 * @param description description of the state
		 * @author Patrick Nicolas
		 * @since August 13, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
case class GA_FAILED(val _description: String) extends GAState(_description)

		/**
		 * <p>Define the RUNNING state for the genetic algorithm-based solver.</p>
		 * @constructor Create a FAILED state for the execution of the genetic algorithm
		 * @author Patrick Nicolas
		 * @since August 13, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
object GA_RUNNING extends GAState("Running")

		/**
		 * <p>Define the NOT RUNNING state for the genetic algorithm-based solver</p>
		 * @constructor Create a FAILED state for the execution of the genetic algorithm
		 * @author Patrick Nicolas
		 * @since August 13, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
object GA_NOT_RUNNING extends GAState("Not Running")

		/**
		 * <p>Define the NON CONVERGENCE state for the genetic algorithm-based solver</p>
		 * <b>description:</b> Description of the state>/p>
		 * @constructor Create a NON CONVERGENCE state for the execution of the genetic algorithm
		 * @param description description of this state
		 * @author Patrick Nicolas
		 * @since August 13, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
case class GA_NO_CONVERGENCE(val _description: String) extends GAState(_description)


// ----------------------------  EOF ------------------------------