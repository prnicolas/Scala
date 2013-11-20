/**
 * Scala test suite for code used in slides "Advanced Scala Functional Programming" Currying.
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * @author Patrick Nicolas
 * @date October 7, 2013
 */
package algorithms

import org.scalatest.FunSuite

class CurryingSuite extends FunSuite {
   test("Alternative foldLeft implementation")  {
     
     val myCollection = new CurriedCollection[Int](Array[Int](3, 5, 8))
     val sum = 0
     val sum1 = myCollection.foldLeft2[Int](sum, (sum, x) => sum + x)
	 val sum2 = myCollection.foldLeft[Int](0)((sum, x) => sum + x)
	 val sum3 = myCollection.values.sum
	 
	 assert(sum1 == sum2, "Expected " + sum1 + " found " + sum2)
   }
   
   test("Recursive currying") {
     
     assert(Math.abs(RecursiveCurrying.f(2.0, 3.0) - 121.0) < 0.5, "Failed 2 arguments recursive expected 121 and found " +RecursiveCurrying.f(2.0, 3.0))
	 assert(Math.abs(RecursiveCurrying.f(Array[Double](2.0, 3.0, 5.0))) < 1.0, "Failed 3 arguments recursive expected 4356 and found " +RecursiveCurrying.f(2.0, 3.0))
   }
   
}

// -------------------  EOF ---------------------------------------------