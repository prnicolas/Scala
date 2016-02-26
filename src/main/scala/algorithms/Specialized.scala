/**
 * Evaluation and comparison of solution to process type erasure.
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * @author Patrick Nicolas
 * @date October 11, 2013
 * @see http://patricknicolas.blogspot.com
 */
case class Item

	/**
	 * Class that illustrates the issue of type erasure in the JVM. The parameterized type List[String] and
	 * List[Int] are not preserved by the compiler and therefore will throw a warning message.
	 * Changes in PN2
	 * @author Patrick Nicolas
	 * @param xs  list of type U bounded by Item
	 * @param f implicit conversion from any parameterized type to its ordered equivalent to support compare.
	 */
class ListCompare[U <: Item](val xs: List[U])(implicit f: U => Ordered[U]) {
   require(xs != null && xs.size > 0, "Cannot create a list comparison with undefined list")
   
   	/**
   	 * Compare this list to another list.
   	 * @param xso other list to be compare to
   	 */
   def compare(xso: List[U]): Boolean = {
	 require(xso != null && xso.size > 0, "ListCompare: Cannot compare this list with an undefined list")
	 
	 xso match {
	    case str: List[String] =>  if( xs.size == xso.size)  xs.zip(xso).exists( x=> x._1.compareTo(x._2) != 0) else false
	  	case n: List[Int] => if( xs.size == xso.size) xs.zip(xso).exists(x => x._1!=x._2) else false
	  	case _ => false
	 }
   }
}

	/**
	 * Class that illustrates the application of manifest to preserve the erasure for the types List[String] and
	 * List[Int]. Manifest can be either provided as implicit argument or as context bounds
	 * @author Patrick Nicolas
	 * @param xs  list of type U bounded by Item
	 * @param f implicit conversion from any parameterized type to its ordered equivalent to support compare.
	 */
class ListCompare2[U <: Item](val xs: List[U])(implicit f: U => Ordered[U]) {
   require(xs != null && xs.size > 0, "ListCompare2: Cannot create a list comparison with undefined list")

   def compare(xso: List[U])(implicit u: Manifest[List[U]]): Boolean = {
	 require(xso != null && xso.size > 0, "ListCompare: Cannot compare this list with an undefined list")
	   
	 if( u <:< manifest[List[String]] ) 
		 if( xs.size == xso.size)  xs.zip(xso).exists( x=> x._1.compareTo(x._2) != 0) else false
	 else if(  u <:< manifest[List[Int]])  
		 if( xs.size == xso.size) xs.zip(xso).exists(x => x._1!=x._2) else false
	 else 
		 false
   }
}


	/**
	 * Class that illustrates the used of specialized primitive type to perserve the erasure for the types 
	 * List[String] and List[Int]
	 * @author Patrick Nicolas
	 * @param xs  list of type U bounded by Item
	 * @param f implicit conversion from any parameterized type to its ordered equivalent to support compare.
	 */
import scala.specialized
class ListCompare3[@specialized Int, String, U <: Item](val xs: List[U])(implicit f: U => Ordered[U]) {
  require(xs != null && xs.size > 0, "ListCompare3: Cannot create a list comparison with undefined list")
	   
  def compare(xso: List[U]): Boolean = {
	require(xs != null && xs.size > 0, "ListCompare3: Cannot create a list comparison with undefined list")
	if( xs.size == xso.size)  xs.zip(xso).exists( x=> x._1.compareTo(x._2) != 0) else false
  }
}
