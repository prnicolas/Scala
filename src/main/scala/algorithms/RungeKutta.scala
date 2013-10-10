/**
 * @author Patrick Nicolas
 * @date June 6, 2013
 */




object RungeKuttaForms extends Enumeration {	
   val RK3 = Array(Array[Double](0.0, 0.0,  1/3,  0.0,  0,0), 
			       Array[Double](0.5, 0.5,  0.0,  2/3,  0.0),
			       Array[Double](1.0, 0.0, -1.0,  0.0,  1/3))
			       
   val RK4 = Array(Array[Double](0.0, 0.0, 0.1666, 0.0,  0,0,  0.0), 
			       Array[Double](0.5, 0.5, 0.0, 0.3333,  0.0,  0.0 ),
			       Array[Double](0.5, 0.0, 0.5, 0.0, 0.3334,  0.0),
			       Array[Double](1.0, 0.0, 0.0, 1.0,  0.0,  0.1667))
			             
   val RKFELBERG = Array(Array[Double](0.0,    0.0,        25/216,    0.0,        0.0,        0.0,        0.0, 0.0), 
			             Array[Double](0.25,   0.25,       0.0,       0.0,        0.0,        0.0,        0.0, 0.0 ),
			             Array[Double](3/8,    3/32,       0.0,       0.0,        1408/2565,  0.0,        0.0, 0.0),
			             Array[Double](12/13,  1932/2197, -7200/2197, 7296/2197,  0.0,        2197/4101,  0.0, 0.0),
			             Array[Double](1.0,    439/216,   -8.0,       3680/513,  -845/4104,   0.0,       -1/5, 0.0),
			             Array[Double](0.5,   -8/27,       2.0,      -3544/2565,  1859/4104, -11/40,      0.0, 0.0))
}



	/**
	 * Configurable solver of Ordinary Differential Equation using Runge-Kutta method of 3rd, 4th and 5th order
	 * @param rungeKuttaForm  form or instance of the Runge-Kutta family of formulas used to resolve ODE
	 * @param adjustStep  local function used to adjust the integration step
	 * @param adjustParameters  parameters used to compute the first order derivative
	 */
class ODESolver(val rungeKuttaForm : Array[Array[Double]],
				val adjustStep : (Double, AdjustParameters) => (Double),
				val adjustParameters : AdjustParameters) {
  
	require( rungeKuttaForm != null, "The form of Runge-Kutta formula is undefined")
	require( adjustStep != null, "The integration step adjusting method is undefined")
	require( adjustParameters != null, "Parameters to compute the first order derivative are undefined")
	
	final class StepIntegration(val coefs : Array[Array[Double]] ) { 
		require( coefs != null && coefs.length > 0, "Cannot solve ODE with undefined coefs")
		
			/**
			 * Compute the integral of the derivative over a predefined interval [x, x+dx] and an integration step
			 * @param x  x-coordinate of point to compute the derivative
			 * @param y  y-coordinate of data point to compute the derivative
			 * @param dx  step of integration 
			 * @param derivative local function that defined the derivative
			 */
		def compute(x: Double, y: Double, dx: Double,			   	 
				    derivative : (Double, Double) => Double) : Double = {
              require( dx > 1e-10 && dx < 10.9, "Integration step " + dx + " is out of bounds")
              require( derivative != null, "Derivative function is undefined")
              
		      val ks = new Array[Double](coefs.length)
		      
		      @scala.annotation.tailrec
		      def compute(i: Int, k: Double, sum: Double) : Double= {
		     	  ks(i) = k
		     	  val sumKs = (0 until i).foldLeft(0.0)((sum, j) => { sum + ks(j)*coefs(i)(j+1) })
		     	  val newK = derivative(x + coefs(i)(0)*dx, y + sumKs*dx)
		     	  if( i >= coefs.size)
		     	 	   sum + newK*coefs(i)(i+2)
		     	  else 
		     	       compute(i+1, newK, sum + newK*coefs(i)(i+2))
		      }
		       
			  compute(0, 0.0, 0.0)*dx
		}	
	}

		/**
		 * Method that resolve the Ordinary Differential Equation by integrating a derivative
		 * function within a range [xBegin, xEnd]. The integration step is automatically recomputed
		 * @param xBegin starting value of the integration range
		 * @param xEnd end of the integration range
		 * @param derivative  local function (x,y) => f(x,y) that defined the derivative formula
		 */
	def solve(xBegin: Double, xEnd: Double, derivative: (Double, Double) => Double) : Double = {
	    require( derivative != null, "Derivative function is undefined")
	    require( Math.abs(xBegin - xEnd) > 1e-10, "Integration interval " + (xEnd - xBegin) + " is too small")
	    
		val rungeKutta = new StepIntegration(rungeKuttaForm)
		
		@scala.annotation.tailrec
		def solve(x: Double, y: Double, dx: Double, sum: Double): Double = {
			val z: Double = rungeKutta.compute(x, y, dx, derivative)
		    if( x >= xEnd)
		    	sum + z
		    else {
		    	val dx = adjustStep(z - y, adjustParameters)
		    	solve(x + dx, z, dx, sum+z)
		    }
		}
	    solve(xBegin, 0.0,  adjustParameters.initial, 0.0)
    }
	
}


case class AdjustParameters(val maxDerivativeValue : Double = 0.01,
							val minDerivativeValue : Double = 0.00001,
							val gamma : Double = 1.0) {
	val initial = 0.5*(maxDerivativeValue + minDerivativeValue)
}


// ------------------------------------  EOF -----------------------------------------------
