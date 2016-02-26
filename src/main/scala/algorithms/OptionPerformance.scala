/**
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * PN2 ===
 * @author Patrick Nicolas
 * @date June 17, 2013
 * 
 * Test driver to evaluate the relative performance of the Option monad comparatively to
 * error code and Exceptions
 */
package algorithms


    /**
    * This a master change on PN2
    */
object OptionPerformance extends App {
	def divOption(x : Double, y : Double) : Option[Double] = {
		 if( Math.abs(y) < 1e-10) None else Some(x/y)
	}
	
	def divException(x : Double, y : Double) : Double = {
		 if( Math.abs(y) < 1e-10) 
			 throw new ArithmeticException("Cannot divide by 0")
		 x/y
	}
	
	def divErrorCode(x : Double, y : Double) : Double = {
		if( Math.abs(y) < 1e-10) Double.NaN else x/y
	    
	}
	
	def testRun(x : Double, y : Double, numIteration : Int ) : Unit = {
		var startTime = System.currentTimeMillis
		for( i <- 0 until numIteration)  {
		    divOption(x,y) match {
		    	case Some(result) => result
		    	case None => None
		    }
		}
		printf("Option[]: %d\n", System.currentTimeMillis - startTime)
		startTime = System.currentTimeMillis
	    for( i <- 0 until numIteration)  {
	    	try {
		    	val result = divException(x,y)
	    	}
	    	catch {
	    		case e : ArithmeticException => 
	    	}
		}
		printf("Exception: %d\n", System.currentTimeMillis - startTime)
	    startTime = System.currentTimeMillis
	    for( i <- 0 until numIteration)  {
	    	val result = divErrorCode(x,y)
	    	if( result.isNaN) {
	    		 1 == 1
	    	}
	    }
	    printf("ErrorCode: %d\n\n", System.currentTimeMillis - startTime)
	}
	
	testRun(2.5, 1.6, 20000)
	
	println("\nBaseline")
	for( i <- 1 until 10) {
	  testRun(2.5, 1.6, 200000*i)
	}
	println("\nError")
    for( i <- 1 until 10) {
	  testRun(2.5, 0.0, 200000*i)
	}
}

// ---------------------------------------  EOF ----------------------------------------