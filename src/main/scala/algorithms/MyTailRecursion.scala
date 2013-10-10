/**
 * @author Patrick Nicolas
 * @date September 4, 2013
 */


	/**
	 * Class that evaluate the performance of tail recursion, comparatively to iterative methods
	 *
	 * f(n) = (n-1)*f(n-1) + (n-2)*f(n-2) + (n-3)*f(n-3)
	 * @param values array of integers used in evaluating the performance of iterative and recursive methods.
	 */
class MyTailRecursion(val values: Array[Int]) {
    require( values != null && values.size > 0, "Failed to apply methods on undefined values")
    
    	/**
    	 * Iterative methods using the scan method used in the comparative evaluation
    	 */
	def iterate1(f: Array[Int] => Int): Array[Int] = {
        require( f != null, "Cannot iterate an undefined local function")
		values.scanLeft(0)((sum, x) => f(values.take(values.indexOf(x)+1))).takeRight(values.size)
    }
    
    
       	/**
    	 * Iterative methods using a simple for loop used in the comparative evaluation
    	 */
	def iterate2(f: Array[Int] => Int): Array[Int] = {
	   require( f != null, "Cannot iterate an undefined local function")
	   
	   val results = new Array[Int](values.size)
	   for( i <- 0 until values.size) {
	  	    val newVal = f(values.take(i+1))
		    results.update(i, newVal)
	   }
	   results
	}
	
		/**
		 * Generic recursive method that implements a tail recursion technique
		 */
	def recurse(f: Array[Int] => Int): Array[Int] = {
	  	require( f != null, "Cannot recurse an undefined local function")
	  	   
		val results = new Array[Int](values.size)
		recurse(f, 0, results)
		
		results
	}
	
	@scala.annotation.tailrec
	private[this] def recurse(f: Array[Int] => Int, cursor: Int, results: Array[Int]): Boolean = {
		if( cursor >= values.size)
			true
		else {
			results.update(cursor, f(values.take(cursor+1)))
		    recurse(f, cursor+1, results)
		}
	}
}


	/**
	 * Test driver
	 */
object MyTailRecursionTest extends App {
	final val WINDOW_SIZE = 3
	
	def polynomial(values: Array[Int]): Int = {
		val arraySlice = if( values.size <= WINDOW_SIZE) values else values.takeRight(WINDOW_SIZE)
		arraySlice.foldLeft[Int](0)((sum, x) => sum + x*values.indexOf(x))
	} 	
	    	
	def performanceEvaluation(numIters: Int) : Unit = {
	    val input = new Array[Int](numIters)
	    for( i <- 0 until input.size) {
	    	 input.update(i, i%3 + 1)
	    }
		val myTailRecursion = new MyTailRecursion(input)
		
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

// ------------------------------------------  EOF ---------------------------------------------
