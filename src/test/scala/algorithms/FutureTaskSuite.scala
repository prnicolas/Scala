/**
 * Scala test suite for code used in post on Futures
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * @author Patrick Nicolas
 */
package algorithms


import org.scalatest.FunSuite

final class FutureTaskSuite extends FunSuite {
    import akka.actor._
    
	test("Default use case") {
        import akka.util.Timeout
        import scala.concurrent.Await
      	implicit val timeout = Timeout(3500)
      	
	    val actorSystem = ActorSystem("FutureTaskSystem")
	    val futureTRef = actorSystem.actorOf(Props( new MainTask), name="myFutureTask")
	    val startTime = System.currentTimeMillis
	    
	    futureTRef tell Launch(200)
	    while ( !futureTRef.isTerminated )  
	       Thread.sleep(50)
	    assert(Math.abs(System.currentTimeMillis - startTime - 1000) < 250, "Computation timing is incorrect")
	    println("Main routine exits")
	}
}



// -------------------------  EOF -----------------------------------------