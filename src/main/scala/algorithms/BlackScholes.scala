/**
 * @author Patrick Nicolas
 * @date April 6, 2013
 */
package algorithms



	/**
	 * Class that implements the Black-Scholes formula for pricing options on underlying securities.
	 * @param maxIters maximum number of iterations to enable convergence of the computation of a call or put
	 * @param errorFactor convergence factor used in the exit condition of the computation of the option value
	 */

class BlackScholes(private val maxIters : Int, private val errorFactor: Double = 1e-4) {
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
		val changeS = (S: Double) => bs1.compute(S,X); call_

        val changer = (r : Double) => {
        	bs2.compute(r,T)
        	bs5.compute(X,r,T)
            call_
        }
        
        val changeSigma = (sigma : Double) => {
        	 bs3.compute(sigma, T)
        	 bs4.compute(sigma, T)
        	 call_
        }
        
        private val call_ = () => {
			val gauss = new Gaussian
			val d1 = (bs1.c + bs2.c + bs3.c)/bs4.c
			S * gauss.value(d1) - bs5.c*gauss.value(d1 -bs4.c)
		}
	}
	
    def callPrice(S : Double, X : Double, r : Double, sigma : Double, T : Double) : Option[Double] = {
        import org.apache.commons.math3.analysis.function.Gaussian
    	
        val d1 = (Math.log(S/X) + (r + 0.5*sigma * sigma) * T) / (sigma * Math.sqrt(T))
        val d2 = d1 - sigma * Math.sqrt(T)
        val gauss = new Gaussian
        Some(S * gauss.value(d1) - X * Math.exp(-r * T) * gauss.value(d2))
    }
    



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

// ---------------------------------------  EOf ----------------------------------------------------