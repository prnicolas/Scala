package algorithms

import org.scalatest.FunSuite

class BlackScholesSuite extends FunSuite {
    
    test("Call option pricing default") {
        computeBS(Array[Double](42.0, 17.0, 5.6, 0.71, 8.2), 3.64)
    }
    
    test("Call option pricing boundary 0.0") {
        val vars = new Array[Double](5) map { _ => 0.0 }
        computeBS(vars, 0.0)
    }
    
    test("Call option pricing boundary -1.0") {
        val vars = new Array[Double](5) map { _ => -1.0 }
        try {
        	computeBS(vars, 3.64)
        }
        catch {
          case e: IllegalArgumentException => assert(true, "Black-Scholes should be out of bounds")
          case e: NullPointerException => assert(false, "Black scholes did not handle illegal argument")
        }
    }
    
    test("Monte Carlo simulation") {
        val S = 124.6
        val X = 12.8
        val r = 7.8
        val sigma = 0.89
        val T = 4.0
        
        new BlackScholes(50).monteCarlo(S,X,r, sigma, T) match {
          case Some(simulatedPrice) => assert(Math.abs(simulatedPrice - 12.7) < 0.1, "Monte Carlo simulator is incorrect")
          case None => assert(false, "Monte Carlo simulation failed")
        }
    }

    
    private[this] def computeBS(vars: Array[Double], targetVal: Double): Unit = {
        new BlackScholes(50).callPrice(vars) match {
           case Some(simulatedPrice) => assert(Math.abs(simulatedPrice - targetVal) < 0.01, "Monte Carlo simulator is incorrect")
          case None => assert(false, "Monte Carlo simulation failed")
        }
    }
}

// ---------------------------------  EOF ---------------------------------------