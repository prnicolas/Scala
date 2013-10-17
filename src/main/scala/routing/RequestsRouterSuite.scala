/**
 * Class to unit test RequestRouter methods.
 * @author Patrick Nicolas
 * @date October 9, 2013
 */

package routing
import org.scalatest.FunSuite
import scala.util.parsing.json.{JSONArray, JSONObject, JSON}


		/**
		 * Scala test class for RequestsRouter
		 */
class RequestsRouterSuite extends FunSuite {

	test("Small-Medium message") {
		val expectedResults = "{message : test1, status : success, routes : [{ip : 10.0.1.0, num : 1, recipients : [+15555555551]}, {ip : 10.0.2.0, num : 5, recipients : [+15555555556, +15555555555, +15555555554, +15555555553, +15555555552]}]}"
		generatedInput(expectedResults, "recipients", List[String]("+15555555556", "+15555555555", "+15555555554", "+15555555553", "+15555555552", "+15555555551"))
	}
	
	
	test("Malformed input") {
	    val expectedResults = "{message : test1, status : success, routes : [{ip : 10.0.1.0, num : 1, recipients : [+15555555551]}, {ip : 10.0.2.0, num : 5, recipients : [+15555555556, +15555555555, +15555555554, +15555555553, +15555555552]}]}"
		generatedInput(expectedResults, "recipes", List[String]("+15555555556", "+15555555555", "+15555555554", "+15555555553", "+15555555552", "+15555555551"))
	}
	
	test("Large sample") {
	    import scala.collection.mutable.ListBuffer	
	    
	    val expectedResults = "{message : test3, status : 0, routes : [{ip : 10.0.4.0, num : 24, recipients : [+15555555556, +15555555557, +15555555558, +15555555559, +15555555560, +15555555561, +15555555562, +15555555563, +15555555564, +15555555565, +15555555566, +15555555567, +15555555568, +15555555569, +15555555570, +15555555571, +15555555572, +15555555573, +15555555574, +15555555575, +15555555576, +15555555577, +15555555578, +15555555579]}, {ip : 10.0.2.0, num : 5, recipients : [+15555555580, +15555555581, +15555555582, +15555555583, +15555555584]}]}"
		val inputMsgs = new ListBuffer[String]
		(1 until 30) foreach( i => { val n : Long = 15555555555L + i; inputMsgs.append("+" + n.toString)})
		generatedInput(expectedResults, "recipients",  inputMsgs.toList)
	}
	
	
	private[this] def generatedInput(expectedResults: String, recipientLabel: String, recipientsList: List[String]) : Unit = { 
	     val jsonArray = new JSONArray(recipientsList)
	     val input = new JSONObject(Map[String, Any]("message"->"test1", "recipients" -> jsonArray))
		  
	     assert(expectedResults.compareTo(RequestsRouter.requestObject(input.toString).getJSONRoutes) ==0)
   }
}