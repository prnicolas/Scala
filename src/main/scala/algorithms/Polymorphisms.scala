/**
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * @author Patrick Nicolas
 * @date August 9, 2013
 * 
 * THe following classes and traits illustrate the different class of polymorphism in Scala.
 */
package algorithms

trait Collection[T <: Double]  {
	 def += (t:T) : Boolean
	 def -= : T 
}

class OrderedCollection[T <: Double] extends Collection[T] {
	import scala.collection.mutable.TreeSet
	
	var buffer = TreeSet.empty(Ordering.fromLessThan[T]((x:Double, y:Double) => (x < y)))
	override def += (t:T) : Boolean = buffer.add(t)
	override def -= : T =  { val top = buffer.head; buffer.remove(top); top }
}

trait Concatenation[M[_]] {
	 def add [T](t:T) : M[T]
	 def pop [T](m:M[T]) : T
}


import scala.collection.GenSeq
import scala.collection.mutable.{ArraySeq, ArrayBuffer}


// This is a new comment for master
trait Bucket[A <: Bucket[A]] extends Seq[A]


object Polymorphisms extends App {
    import scala.collection.mutable.ArrayBuffer
    
    implicit val concatenateArrayBuffer = new Concatenation[ArrayBuffer] {
    	  override def add [T](t:T) : ArrayBuffer[T] =  ArrayBuffer[T](t)
    	  override def pop [T](m:ArrayBuffer[T]) : T = m.head 
    }

	
	def compose[M[_]: Concatenation, X, Y](c1: M[X], c2: M[Y]) = {
		 val c = implicitly[Concatenation[M]]
		 c.add(c.pop(c1), c.pop(c2))
	}
	
	val mx  = compose(ArrayBuffer[Double](2.6), ArrayBuffer[Double](6.6))
	mx.foreach( x => println(x))
}

// -----------------------------  EOF -----------------------------