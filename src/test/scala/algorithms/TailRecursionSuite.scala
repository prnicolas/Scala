/**
 * Scala test suite for evaluating the performance of tail recursion
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * @author Patrick Nicolas
 */
package algorithms

import org.scalatest.FunSuite



final class TailRecursionSuite extends FunSuite {
   test("Performance test case") {
       val WINDOW_SIZE = 3
	
		def polynomial(values: Array[Int]): Int = {
			val arraySlice = if( values.size <= WINDOW_SIZE) values else values.takeRight(WINDOW_SIZE)
			arraySlice.foldLeft[Int](0)((sum, x) => sum + x*values.indexOf(x))
		} 	
		    	
		def performanceEvaluation(numIters: Int) : Unit = {
		    val input = new Array[Int](numIters)
		    for( i <- 0 until input.size) {
		    	 input.update(i, i%3 + 1)
		    }
			val myTailRecursion = new TailRecursion(input)
			
			var startTime = System.currentTimeMillis
			for( i <- 0 until 1000)
			   myTailRecursion.iterate1(polynomial)
		    val firstIter = System.currentTimeMillis-startTime
		    
		    startTime = System.currentTimeMillis
		    for( i <- 0 until 1000)
		      	myTailRecursion.iterate2(polynomial)
		    val secondIter = System.currentTimeMillis-startTime
		    
		    startTime = System.currentTimeMillis
		    for( i <- 0 until 1000)
		         myTailRecursion.recurse(polynomial)
		    val recursion = System.currentTimeMillis-startTime
		    println("Iters: " + numIters + "(" + firstIter + "," + secondIter + "," + recursion + ")")
		}
	    
		for( i <- 10 until 100 by 10)
			performanceEvaluation(i)
	}
}

// -------------------------  EOF ------------------------------------