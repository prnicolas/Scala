/**
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * @author Patrick Nicolas
 * @date May 14, 2013
 * 
 * The following classes evaluates the relative performance of several forms of loops in Scala
 */
package algorithms

import scala.util.Random
import scala.util.Sorting
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder.ofDouble
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer


  // This is a change made in PN-1
class Performance(numIters : Int, size : Int) {
	lazy val foreachLoop : Array[Long] = new Array[Long](numIters)
	lazy val mapLoop : Array[Long] = new Array[Long](numIters)
	lazy val forLoop : Array[Long] = new Array[Long](numIters)
	lazy val leftFoldLoop : Array[Long] = new Array[Long](numIters)
	var data : List[Double] = null 
		

	def compareSummation(count : Int) : Unit = {
		if ( data == null) {
			val dataBuf = new ListBuffer[Double]()
			for( i <- 0 until size) {
				dataBuf.append(0.0)
			}
			data = dataBuf.toList
		}
		val z : Double = 3.45
		var endTime = System.currentTimeMillis
		var newArray = data.foreach( x => z)	
		foreachLoop(count) = System.currentTimeMillis - endTime
		endTime = System.currentTimeMillis()
		newArray = data map { x => z } 
		mapLoop(count) = System.currentTimeMillis - endTime
		endTime = System.currentTimeMillis
		for ( i <- 0 until size) {
			z :: data
		}
		forLoop(count) = System.currentTimeMillis - endTime
		endTime = System.currentTimeMillis()
		newArray = data.foldLeft(z)( (x, z) => z)
		leftFoldLoop(count) = System.currentTimeMillis - endTime
	}	
	
	def autoboxingTest(count : Int) : Unit = {
		val data = new Array[Double](size)
		var endTime = System.currentTimeMillis
		for(  i <- 0 until size) {
			data(i) = 2.0
		}
		foreachLoop(count) = System.currentTimeMillis - endTime
		endTime = System.currentTimeMillis
		for(  i <- 0 until size) {
			data.update(i, 2.0)
		}
		mapLoop(count) = System.currentTimeMillis - endTime
	}
	
	private def specialBoxing[@specialized(Double) T:ClassTag](count : Int, t : T) : Unit = {
		val data = new Array[T](size)
		var endTime = System.currentTimeMillis
		for(  i <- 0 until size) {
			data.update(i, t)
		}
		forLoop(count) = System.currentTimeMillis - endTime
	}
}


final class SpArray[@specialized A:ClassTag](val size : Int) {
	require( size > 2)
	
	val array : Array[A] = new Array[A](size)
	def apply(i : Int) : A = array.apply(i)
	def update(i : Int, a : A) : Unit = array.update(i, a)
}



object TestPerformance {
	val numIterations : Int = 10;
	private def evaluateArrayPerformance(sz : Int) : Unit = {
		val eval = new Performance(numIterations, sz)
		for( i <- 0 until numIterations) {
			eval.compareSummation(i)
		}
		printf("\nSize:%d - foreach:%f - map:%f - for:%f - leftFold:%f", 
				sz,
				eval.foreachLoop.sum.asInstanceOf[Double]/numIterations,
				eval.mapLoop.sum.asInstanceOf[Double]/numIterations,
				eval.forLoop.sum.asInstanceOf[Double]/numIterations,
				eval.leftFoldLoop.sum.asInstanceOf[Double]/numIterations)
	}
	
	private def evaluateAutoBoxing(sz : Int, count : Int) :Unit = {
		val array = new Array[Double](sz)
		var endTime = System.currentTimeMillis
		for( i <- 0 until count) {
			for(  i <- 0 until sz) 
				array(i) = 2.0
		}
		var duration = (System.currentTimeMillis - endTime)
		printf("\nAutoboxing: %f", duration.asInstanceOf[Double]/count)
		
		val spArray = new SpArray[Double](sz)
		endTime = System.currentTimeMillis
		for( i <- 0 until count) {
			for(  i <- 0 until sz) 
				spArray(i) = 2.0
		}
		duration = (System.currentTimeMillis - endTime)
		printf("\nspecialized: %f", duration.asInstanceOf[Double]/count)

	}
	
	object Conversion {
		type BYTES = Array[Byte]
		  implicit def String2ArrayBytes(s:String) : Array[Byte] = {
		 	  var i : Int = 0
		 	  val array = new Array[Byte](s.length)
		 	  while(i < s.length) {
		 	 	   array.update(i, (s.charAt(i) & 0x03).toByte)
		 	 	   i += 1
		 	  }
		 	  array
		  }
		   
		  implicit def byte2String(b: Byte) : String = b.toString
		  
		  implicit def string2Byte(ch:Char) : Byte =  (ch & 0x03).toByte
		   
		  implicit def ArrayBytes2String(array: BYTES) : String = {
		 	   val buf = new StringBuilder
		 	   var i =0
		 	   while( i < array.size) {
		 	  	   buf.append(array.apply(i))
		 	  	   i += 1
		 	   }
		 	   buf.toString
		  }
	}
	
	def main(args : Array[String]) : Unit = {
		import Conversion._
	    
		val bytes = Array[Byte](1,0,0,1)
		val str = new String("1001")
		val s = bytes
		println(s)
		
		var buf= new StringBuilder
		for( i <- 0 until bytes.size)
		   println(String.valueOf(bytes.apply(i)))
		    
		   
		
		for (i <- 0 until str.getBytes.size)  {
			 val b : Byte = (str.charAt(i) & 0x03).toByte
			 println(b.toString)
		}
	}
}