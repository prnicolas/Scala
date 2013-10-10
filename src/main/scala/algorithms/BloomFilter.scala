/**
 * @author Patrick Nicolas
 * @date October 2, 2013
 */
package algorithms

import java.security._
import java.nio.ByteBuffer
import java.math.BigInteger


		/**
		 * Basic and simple implementation of the Bloom filter
		 * @param initialCapacity  capacity of the Bloom filter (size of the bitvector)
		 * @param numHashs number of hash function used to associated an element to a position in the bit vector
		 * @param algorithm hashing algorithms used to compute the position in the bit vector (MD5, SHA-1,...)
		 */
final class BloomFilter(private val initialCapacity: Int, 
					    private val numHashs: Int, 
					   private val algorithm: String) {
	require( initialCapacity > 4, "Bloom filter capacity is too small")
	require( numHashs > 0, "Bloom filter has no hash function")
	
    private[this] val set = new Array[Byte](initialCapacity)
    private[this] var numElements = 0
    private[this] val digest = {
		try {
		  MessageDigest.getInstance(algorithm)
		}
		catch {
			case e: NoSuchAlgorithmException => null
		}
	}
    

    def add(anyArray: Array[Any]): Unit = { 
    	if( digest != null) {
    	   anyArray.foreach( getSet(_).foreach( set(_) = 1) )
    	   numElements += anyArray.size
    	}
    }
    
    @inline
    def add(any: Any): Unit = this.add(Array[Any](any))
    
    @inline
    def contains(any: Any): Boolean = if( digest != null) !getSet(any).exists( set(_) != 1) else false
    
    
    import BloomFilter._
    private[this] def hash(value: Int) : Int = {
	    digest.reset
	    digest.update(value)
	    Math.abs(new BigInteger(1, digest.digest).intValue) % (set.size -1)
    }
    
    private[this] def getSet(any: Any): Array[Int] = {
       val newSet = new Array[Int](numHashs)
	   newSet.update(0, hash(any.hashCode))
	   getSet(newSet, 1)
	   newSet
    }

    @scala.annotation.tailrec
    private[this] def getSet(values: Array[Int], index: Int) : Boolean = {
		if( index >= values.size)
			true
		else {
			values.update(index, hash(values(index-1)))
			getSet(values, index+1)
		}
	}
}

	/**
	 * Companion object used to implement the integer to byte conversion
	 */
object BloomFilter {
    implicit def Int2Bytes(value: Int) : Array[Byte] = {
    	val bytes = new Array[Byte](4)
    	bytes.map( x => { val offset = (bytes.size -1 - bytes.indexOf(x)) <<3; ((value >>> offset) & 0xFF).asInstanceOf[Byte]  })
    	bytes
    }
}

import BloomFilter._
object BloomFilterTest extends App {
	val filter = new BloomFilter(1000, 1000, "SHA")
	final val newValues = Array[Any](57, 97, 91, 23, 67, 33)
	filter.add(newValues)
	
	println( filter.contains(22) )
	println( filter.contains(23) )
}

// --------------------------------------------  EOF -------------------------------------------