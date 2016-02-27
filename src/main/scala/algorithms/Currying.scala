/**
 * Class to illustrate implementation of method currying using foldLeft high order functions
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * Changes made on Master
 * @author Patrick Nicolas
 * @date October 2, 2013
 * @see http://patricknicolas.blogspot.com
 */
package algorithms

/**
	* Class to demonstrate currying in collections
	* His is new
	* @param values
	* @tparam T
	*/
class CurriedCollection[T](val values: Array[T]) {
   def foldLeft2[U](u: U, op: (U,T) => U): U = values.foldLeft[U](u)((u, t) => op(u,t))
   def foldLeft[U](u: U)(op: (U,T) => U): U = this.foldLeft2(u, op)    
}

	/**
	 * Singleton that illustrates the recursive implementation of
	 * variable number of argument  f(x + (f(y) + f(z) + ...   ))))
	 */
object RecursiveCurrying {
	def f(x: Double): Double = x*x
	def f(x: Double, y: Double): Double = f(x + f(y))
	def f(xs: Array[Double]) : Double = 
	   if(xs.size > 1) f(xs.head + f(xs.takeRight(xs.size -1))) else xs(xs.size-1)
}

object Currying extends App {
	val myCollection = new CurriedCollection[Int](Array[Int](3, 5, 8))
	def sum(a: Int)(b: Int): Int = a+b
	def sums(a: Int): Int => Int = b => sum(a)(b)
	assert(sum(3)(5) == sums(3)(5), "Expected " + sums(3)(5) + " found " + sums(3)(5))

	
	println(RecursiveCurrying.f(2.0, 3.0))
	println(RecursiveCurrying.f(Array[Double](2.0, 3.0, 5.0)))
}

// ----------------  EOF ---------------------------------------------------------