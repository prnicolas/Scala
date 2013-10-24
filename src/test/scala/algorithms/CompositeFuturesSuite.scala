/**
 * Scala test suite for composable futures
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * @author Patrick Nicolas
 */
package algorithms


import org.scalatest.FunSuite

class CompositeFuturesSuite extends FunSuite {
  
   test("Primary test case") {
      val result =  new CompositeFutures(2, 2).compositeFuture
      assert( result  == 57, "Futures composition result: " + result + " is incorrect")
   }
}

// ---------------------------  EOF ---------------------------------------