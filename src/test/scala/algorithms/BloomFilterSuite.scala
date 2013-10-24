/**
 * Scala test suite for BloomFilter
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * @author Patrick Nicolas
 */
package algorithms


import org.scalatest.FunSuite


class BloomFilterSuite extends FunSuite {
  import BloomFilter._
      
  test("Basic use case") {

	val filter = new BloomFilter(1000, 1000, "SHA")
	val newValues = Array[Any](57, 97, 91, 23, 67, 33)
	filter.add(newValues)
	
	assert( !filter.contains(22), "failed to detect that 22 is not part of the filter")
	assert( filter.contains(23),  "failed to detect that 23 is part of the filter")
  }
  
  test("Boundary case: empty array") {
    val filter = new BloomFilter(1000, 1000, "SHA")
    try  {
    	filter.add(Array[Any]())
    	assert(false, "Failed to detect zero-length array for Bloom filter")
    }
    catch {
       case e: IllegalArgumentException => println("Failed to detect zero-length array for Bloom filter")
    }
  }
  
  test("Boundary case: null element") {
    val filter = new BloomFilter(1000, 1000, "SHA")
    try  {
    	filter.add(null)
        assert(false, "Failed to detect adding a null element in Bloom filter")
    }
    catch {
       case e: IllegalArgumentException => println("Failed to detect zero-length array")
    }
  }
  
  test("Boundary case: incorrect parameters for instantiation") {
    try  {
    	new BloomFilter(1000, 0, "SHA")
        assert(false, "Failed to detect incorrect argument for Bloom Filter constructor")
    }
    catch {
       case e: IllegalArgumentException => println("Failed to detect zero-length array")
    }
    
    try  {
    	new BloomFilter(3, 5, "SHA")
        assert(false, "Failed to detect incorrect argument for Bloom Filter constructor")
    }
    catch {
       case e: IllegalArgumentException => println("Failed to detect zero-length array")
    }
    
    try  {
    	new BloomFilter(1000, 2000, "SHA")
        assert(false, "Failed to detect incorrect argument for Bloom Filter constructor")
    }
    catch {
       case e: IllegalArgumentException => println("Failed to detect zero-length array")
    }
  }
}

// ------------------------------  EOF ------------------------------------------