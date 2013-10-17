/**
 * Class that generates contacts randomly over a sequence of area code
 * @author Patrick Nicolas
 * @date Oct 11, 2013
 */
package routing

import scala.util.Random
import scala.collection.mutable.ArrayBuffer


		/**
		 * Class to generate randomly a number of phone numbers
		 * @param numContacts Number of contacts 
		 * @throw IllegalArgumentException if the number of contacts is not a positive value
		 */
import ContactsGenerator._
class ContactsGenerator(private val numContacts: Int) {
    require( numContacts >0, "Number of contacts has to be a positive number")
    
	def generate : Array[String] = {
	    val randomGenerator = new Random
	    (0 until numContacts).foldLeft[ArrayBuffer[String]](new ArrayBuffer[String])(  (buf, n) => {
	         buf.append( areaCodes(n%areaCodes.size) + (randomGenerator.nextInt(9000000) + 1000000).toString) 
	         buf
	    }).toArray
	}
}


		/**
		 * Companion object for the area code
		 */
object ContactsGenerator {
   val areaCodes = Array[String]("415", "408", "650")
}

// -------------------------  EOF ----------------------------------------------------------------------