/**
 * Scala test suite for code used in slides "Advanced Scala Functional Programming"
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * @author Patrick Nicolas
 * @date October 7, 2013
 */
package algorithms

import org.scalatest.FunSuite


final class ContinuationSuite extends FunSuite {
   test("Simple continuation") {
      val computation = new Computation((x:Double) => Math.sqrt(x))
      val fcont = computation.continue((f: Double => Double)  => ((x:Double) => Math.sin(f(x))))
  
      val error =  Math.abs( fcont(2.0) - Math.sin(Math.sqrt(2.0)) )
      assert( error < 0.01, "Expected " +  Math.sin(Math.sqrt(2.0)) + " found " +  Math.abs( fcont(2.0)) )
   }
   
   test( "Nested continuation") {
      val computation = new Computation((x:Double) => Math.sqrt(x))
        
      val value = computation.single(2.0)
      assert( Math.abs(value - 5.657) < 0.05, "Expected 5.657 found " +  value)
      
      val value2 = computation.single(2.0)
      assert( Math.abs(value2 - 5.412) < 0.05, "Expected 5.412 found " +  value2)
   }
}

// --------------------------------  EOF ------------------------------------