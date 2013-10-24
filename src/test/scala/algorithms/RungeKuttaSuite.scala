/**
 * Scala test suite for Runge-Kutta family of algorithms to resolve ODEs
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * @author Patrick Nicolas
 */
package algorithms

import org.scalatest.FunSuite

final class RungeKuttaSuite extends FunSuite {
	test("Main use case") {
	   val area = computeArea( (x: Double, y: Double) => 2.0*x*x)
	   assert(Math.abs(area - 34.7) < 0.5,"Failed to resolve Runge-Kutta: Found " + area + " require 34.7" )
	}
	
	test("Boundary case derivative zero") {
	   val area = computeArea( (x: Double, y: Double) => 0.0)
	   assert(Math.abs(area - 0.0) < 0.5,"Failed to resolve Runge-Kutta: Found " + area + " require 0.0" )
	}
	 
	test("Boundary case derivative divide by zero") {
	  try  {
	      computeArea( (x: Double, y: Double) => 1.0/(x - 1.0))
	  }
	  catch {
	    case e: ArithmeticException => assert(true, "Did not detect divide by zero") 
	  }
    }

	private[this] def computeArea(f: (Double, Double) => Double) : Double = {
	   	val odeSolver = new RungeKutta("RK4",  AdjustParameters(0.1, 0.01, 0.02))
	     odeSolver.solve(1.0, 5.0, f)
	}
}

// ------------------------------  EOF ------------------------