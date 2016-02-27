/**
 * Classes that illustrates the use of delimited continuation in Scala
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * Note: The compiler require the CPS plug-in, using the continuations=enable in the command line
 * Changes made on Master
 * @author Patrick Nicolas
 * @date October 7, 2013
 * @see http://patricknicolas.blogspot.com
 */
package algorithms

final class Computation( val f: Double=>Double) {
  import scala.util.continuations._
	
  require(f != null, "cannot extend computation of undefined function")
  
  val identity = (h: Double => Double) => h

  def continue(cont: (Double=>Double) => (Double=>Double) = identity): Double => Double = cont(f)
   
  def single(x: Double) = {
	  reset {
		  shift {h: (Double => Double) => h(x)*f(x) 
	       } + x
	   }
   }
   
      
   def nested(x: Double) = {
	   reset {
		   shift {h: (Double => Double) => { reset { shift { hh: (Double => Double) => h(hh(x*x))}} }
	       } + f(x)
	   }
   }
}


// ------------------------  EOF -----------------------------------------