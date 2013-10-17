/**
 * Main class to route request to the appropriate service according to the payload. The
 * service is defined by its IP address
 * 
 * @author Patrick Nicolas
 * @date October 6, 2013
 */

package routing
import scala.util.parsing.json.{JSONArray, JSONObject, JSON}
import scala.collection.mutable.ListBuffer



		/**
		 * Class that implements routers of requests for different IP targets. The main method, getRouter
		 * use a tail recursion to assign the messages of the request to the appropriate IP addresses
		 * @param inputRequest  JSON request 
		 */
import RequestsRouter._



		/**
		 * Class to route a message containing a list of contact information
		 * (phone numbers)
		 * @author Patrick Nicolas
		 * @param messageName name of the message
		 * @param contactInfo array of contacts information
		 * @throw IllegalArgumentException if the contact Information is empty
		 */
final class RequestsRouter(private val messageName: String, 
						   private val contacts: Array[String]) {
  
    require(contacts != null && contacts.size > 0, "Cannot route undefined list of contacts")
    val requestObject : (String, Array[String]) = new Tuple2(messageName, contacts)
     
    def getJSONRoutes: String = 
    	if( requestObject == null ) 
    	    "Improperly formatted JSON input" 
    	else 
    		JSONoutput(getRouteMap, requestObject._1).toString
    
    		
    		/**
    		 * Retrieve the map of IP -> Contacts list using a tail recursion on
    		 * a local function.
    		 */
    def getRouteMap: Map[String, Array[String]] = {
       import scala.collection.mutable.HashMap
       val routingMap = new HashMap[String, Array[String]]
    	 
       @scala.annotation.tailrec
	   def route(currentMessages: Array[String], index: Int, requestsType: List[(Int, String)]) : Int = {
    	   if( index >= requestsType.size) 
    	  	   index

    	  else {
			   val numMessagesForIP = (currentMessages.size/(requestsType(index)._1.toDouble))
			   if( numMessagesForIP >= 1.0) {
			       val numberMessageForIP = requestsType(index)._1*numMessagesForIP.floor.toInt
			   	   routingMap.put(requestsType(index)._2, currentMessages.slice(0,  numberMessageForIP))
			   	   route(currentMessages.slice(numberMessageForIP, currentMessages.size), index+1, requestsType)
			   }
			   else 
			       route(currentMessages, index+1, requestsType)
    	   }
	   }
       route(requestObject._2, 0, requestsType)
       routingMap toMap
    }
    
        /**
         * Generate the JSON input string in case the input is provided through a table inGUI
         */
    def JSONInput: String = {
        val jsonPhoneNumbers = new JSONArray(contacts.toList)
        new JSONObject(Map[String, Any]( "message" ->messageName, "recipients" -> jsonPhoneNumbers) ).toString
    }

    
    private[this] def JSONoutput(routingTable: Map[String, Array[String]], messageName: String ): JSONObject = {
         val routesList = new ListBuffer[JSONObject]
         routingTable.foreach( kv => { 
        	   val jsonRecipientsPerIP = new JSONArray(kv._2.toList)
        	   val jsonObject = new JSONObject(Map[String, Any](
        	  		   "ip" -> kv._1, "num" -> kv._2.size, "recipients"->jsonRecipientsPerIP)  )
        	   routesList.append(jsonObject)
         })
         
         new JSONObject(Map[String, Any] ( "message" -> messageName, "status"->NO_ERROR, "routes" -> new JSONArray(routesList.toList)))
    }
    
    private[this] def output(msgName: String): JSONObject = {
    	 new JSONObject(Map[String, Any] ( "message" -> msgName, "status"->ERROR_INPUT, "routes" -> "[]"))
    }
    
    override def toString : String = {
        new StringBuilder(requestObject._1).
                   append("[").
                      append(requestObject._2.foldLeft(new StringBuilder)((buf, t) => buf.append(t).append(",")).toString).
                        append("]").toString        
    }
}


		/**
		 * Companion object
		 */

object RequestsRouter {
   final val NO_ERROR = 0
   final val ERROR_INPUT = 1
   final val ERROR_PROC = 2
   
   val requestsType = List[(Int, String)](
		25 -> "10.0.4.0",
		0x0A -> "10.0.3.0",
		0x05 -> "10.0.2.0",
		0x01 -> "10.0.1.0")
		
		
    def requestObject(inputRequest: String): RequestsRouter = {
    	JSON.parseFull(inputRequest) match {
    	     case Some(objectMap ) => {  
    	    	objectMap.asInstanceOf[Map[String, Any]]("message") match {
    	        	 case name: String => {
    	                 objectMap.asInstanceOf[Map[String, Any]]("recipients") match {  
    	      	             case l => new RequestsRouter(name, l.asInstanceOf[List[String]].toArray[String])
    	      	             case _ => new RequestsRouter(name, null)
    	                 }
    	      	     }
    	      	     case _ => null
    	        }
    	     }
    	     case None => null
    	}
    }
}

// ----------------------------------------------- EOF ------------------------------------------------------------------------------------------