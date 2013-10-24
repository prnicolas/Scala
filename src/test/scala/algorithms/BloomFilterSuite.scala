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
  test("Basic use case") {
    import BloomFilter._
	val filter = new BloomFilter(1000, 1000, "SHA")
	val newValues = Array[Any](57, 97, 91, 23, 67, 33)
	filter.add(newValues)
	
	assert( !filter.contains(22), "failed to detect that 22 is not part of the filter")
	assert( filter.contains(23),  "failed to detect that 23 is part of the filter")
  }
}

// ------------------------------  EOF ------------------------------------------