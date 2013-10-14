/**
 * Classes that illustrates the use of Futures in Scala 2.10.2 and Akka 2.14 framework. The pattern consists of a 
 * main task, 'MainTask' that launches than blocks on a future task 'FutureTask' 
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


case class Launcher(val numIteration : Int)
case class Receiver

		/**
		 * Create a task with a future computation defiend as a reference
		 * @param futureTask  reference to the actor th
		 */
class MainTask(val futureTask : ActorRef) extends Actor {
	require( futureTask != null, "Undefined reference to future task or actor")
	
	implicit val timeout = Timeout(3500)
	 
	override def receive = {
		case launch : Launcher => {
			Thread.sleep(launch.numIteration)
			val f = futureTask ? new Receiver
			val results = Await.result(f, timeout.duration).asInstanceOf[String]
			exit
		}
		case _ => println("Undefined message")
	}
}


class FutureTask extends Actor {
	override def receive = {
		case receive : Receiver  => {
			Thread.sleep(1000)
			sender ! "Done"
		}
		case _ => println("Failed handler")
	}
}

// ------------------------  EOF -----------------------------------------------------------------------------