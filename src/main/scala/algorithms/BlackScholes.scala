/**
 * Simple implementation of the Black-Scholes formula to price call options on underlying security. 
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * @author Patrick Nicolas
 * @date April 6, 2013
 * @see  http://patricknicolas.blogspot.com
 */
package algorithms



	/**
	 * Class that implements the Black-Scholes formula for pricing options on underlying securities.
	 * @param maxIters maximum number of iterations to enable convergence of the computation of a call or put
	 * @param errorFactor convergence factor used in the exit condition of the computation of the option value
	 */

import BlackScholes._
class BlackScholes(private val maxIters : Int, private val errorFactor: Double = EPS) {
   require( maxIters > 0 && maxIters < 1000, "Maximum number of iterations, " + maxIters.toString + " is incorrect" )
   require(errorFactor > 1e-10 && errorFactor < 1e-2, "Error factor, " + errorFactor + " used in Black-Scholes is out-of-boundas")
   
   def bs1f(x: Double*): Double = Math.log(x(0)/x(1))
   val bs1 = new BlackScholesStep(bs1f)
		
   def bs2f(x: Double*): Double = x(0)*x(1)
   val bs2 =  new BlackScholesStep(bs2f)
		
   def bs3f(x: Double*): Double = 0.5*x(0)*x(0)*x(1)
   val bs3 =  new BlackScholesStep(bs3f)
		
   def bs4f(x: Double*): Double = x(0) * Math.sqrt(x(1)) 
   val bs4 =  new BlackScholesStep(bs4f)
		
   def bs5f(x: Double*): Double = - x(0) * Math.exp(-x(1)*x(2)) 
   val bs5 =  new BlackScholesStep(bs5f)
		
   		/**
   		 * Inner class to simulate the pricing of a call option
   		 */
   class CallPriceSimulator(val S: Double, val X: Double, val r: Double, val sigma: Double, val T: Double) {
		import org.apache.commons.math3.analysis.function.Gaussian

		val call = () => {
		    bs1.compute(S,X)
			bs2.compute(r,T)
			bs3.compute(sigma,T)
			bs4.compute(sigma,T)
			bs5.compute(X,r,T)
			call_
		}
		
		@inline
		protected[this] val changeS = (S: Double) => { bs1.compute(S,X); call_ }

        protected[this] val changer = (r : Double) => {
        	bs2.compute(r,T)
        	bs5.compute(X,r,T)
            call_
        }
        
        protected[this] val changeSigma = (sigma : Double) => {
        	 bs3.compute(sigma, T)
        	 bs4.compute(sigma, T)
        	 call_
        }
        
        private[this] val call_ = () => {
			val gauss = new Gaussian
			val d1 = (bs1.c + bs2.c + bs3.c)/bs4.c
			S * gauss.value(d1) - bs5.c*gauss.value(d1 -bs4.c)
		}
	}
	
    def callPrice(variables: Array[Double]): Option[Double] = {
        require(variables != null && variables.size == 5, "Cannot compute call price with undefined variables")
        this.callPrice(variables(0), variables(1), variables(2), variables(3), variables(4))
    }
   
    def callPrice(S: Double, X: Double, r: Double, sigma: Double, T: Double): Option[Double] = {
        import org.apache.commons.math3.analysis.function.Gaussian
    	require(S >= 0.0, "S: " +S.toString + " is out of bounds")
        require(X >= 0.0, "S: " +S.toString + " is out of bounds")
        require(r >= 0.0, "S: " +S.toString + " is out of bounds")    	
        require(sigma >= 0.0, "S: " +S.toString + " is out of bounds")
        require(T >= 0.0, "S: " +S.toString + " is out of bounds")
            	
        if( S < EPS || X < EPS || r < EPS || sigma < EPS || T < EPS)
            Some(0.0)
        
        else {
	        val d1 = (Math.log(S/X) + (r + 0.5*sigma * sigma) * T) / (sigma * Math.sqrt(T))
	        val d2 = d1 - sigma * Math.sqrt(T)
	        val gauss = new Gaussian
	        Some(S * gauss.value(d1) - X * Math.exp(-r * T) * gauss.value(d2))
        }
    }
    


    	/**
    	 * <p>Method that implement the monteCarlo simulation for the Gaussian distribution</p>
    	 */
    def monteCarlo(S : Double, X :Double, r : Double, sigma : Double, T : Double) : Option[Double] = {
        import scala.util.Random
        
        val rGen = new Random(System.currentTimeMillis)
        val alpha1 = Math.exp(r*T)
        val alpha2 = S*Math.exp(- 0.5*sigma*sigma*T)
        val theta = alpha1*alpha2
        val delta = Math.exp( sigma*Math.sqrt(T))	

        val totalValues = (0 until maxIters).scanLeft(0.0)( (sum, n) => { 
        	 rGen.setSeed(rGen.nextLong)
             val price = theta*Math.pow(delta, rGen.nextGaussian)
             val value = if( price < X) 0 else price - X
             value
        }).takeWhile( _ > 0)
        
        if( totalValues.size < maxIters)  Some(alpha1 *totalValues.last / maxIters) else None
    }
}


	/**
	 * Class that implements the step size process for the Black-Scholes formula.
	 */
final class BlackScholesStep(val f : (Double*) => Double, var c : Double = 0.0) {
	def compute(x:Double, y:Double) : Unit = { c = f(x, y) }
	def compute(x:Double, y:Double, z:Double) : Unit = { c = f(x, y, z) }
}


object BlackScholes {
   final val EPS = 1e-4
}

// ---------------------------------------  EOf ----------------------------------------------------