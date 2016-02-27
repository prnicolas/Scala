/**
 * Classes that illustrates the use of Futures in Scala 2.10.2 and Akka 2.14 framework. The pattern consists of a 
 * main task, 'MainTask' that launches than blocks on a future task 'FutureTask' 
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * Master changes
 * @author Patrick Nicolas
 * @date July 23, 2013
 * 
 */
package algorithms

import akka.actor.{Actor, ActorSystem, ActorRef, Props}
import akka.util.Timeout
import akka.pattern.ask
import scala.concurrent.{Await, Future, Promise}
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global


	/**
	 * Messages
	 */
case class Launch(val numIterations: Int)
case class Receiver

		/**
		 * Create a task that trigger a another task/actor to simulate future
		 */
class MainTask extends Actor {
	
	implicit val timeout = Timeout(3500)
	val futureTaskRef: ActorRef = context.actorOf(Props(new FutureTask("myTask", 500)), name="MyTask")
	var completed = false
	
	override def receive = {
		case launch: Launch => {
			Thread.sleep(launch.numIterations)
			futureTaskRef ! new Receiver
		}
		case "Done" => {
		     context.stop(futureTaskRef)
		     context.stop(self)
		}
		case _ => println("Undefined message")
	}
	
	override def postStop: Unit = println("Main task exits")
}


class FutureTask(val myName: String, val mySleep: Long) extends Actor {
    require(mySleep > 200 && mySleep < 5000, "Sleeping duration is out of bounds")
    
	override def receive = {
		case receive: Receiver  => {
			Thread.sleep(mySleep)
			sender ! "Done"
		}
		case _ => println("Failed handler")
	}
	
    override def postStop: Unit = println("Future task exits")
}



// ------------------------  EOF -----------------------------------------------------------------------------