/**
 * @author Patrick Nicolas
 * @date July 23, 2013
 * 
 * Classes that illustrates the use of Futures in Scala and Akka framework
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
		 * Create a task with a future
		 */
class MyTask(val futureTask : ActorRef) extends Actor {
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


final class PowerToN(val sleepDuration : Long, val x : Int, val n : Int = 2) {
    require( sleepDuration > 100L && sleepDuration < 20000L, "Sleep duration " + sleepDuration + " is out of bounds")
    
	implicit val timeout = Timeout(sleepDuration*5)

	def singleFuture : Unit = {	
		 val powN = Future[Int] {
			 Thread.sleep(sleepDuration)
			 var product: Int = x
			 for(i <- 1 until n) {
				 product *= x
			 }
			 
			 product
		 }		 

		val results = Await.result(powN, timeout.duration).asInstanceOf[Int]
		println("Results=" + results)
	}
	
	def compositeFuture : Int = {
		val startTime : Long = System.currentTimeMillis

		val sumN = Future[Int]  {
			println("SumN enter")
			val startTime : Long = System.currentTimeMillis
			Thread.sleep(sleepDuration)
			
			var sum : Int = x
			for(i <- 1 until n) {
				sum += x
			}
			println("sumN(" + x + ")=" + sum + " after " +  (System.currentTimeMillis() - startTime).toString)
			sum
		}
		val expN = Future[Int] {
			val startTime : Long = System.currentTimeMillis
			Thread.sleep(sleepDuration*2)
			val expValue : Int = Math.exp(x*n).ceil.toInt
			println("expN(" + x + ")=" + expValue + " after " +  (System.currentTimeMillis - startTime).toString)
			expValue
		}
		val divN = Future[Int] {
			println("divide enter")
			val startTime : Long = System.currentTimeMillis
			Thread.sleep(sleepDuration>>1)
			val divide : Double = x / n
			println("divN(" + x + ")=" + divide + " after " +  (System.currentTimeMillis - startTime).toString)
			divide.toInt
		}
	
		
		val fut: Future[Int] = for {
			z <- sumN
			y <- expN
			t <- divN
		} yield z+y+t
		
				
		var result : Int = 0
		fut onSuccess {
			case value : Int => println("Result=" + value); result = value
			println("Completed after " + (System.currentTimeMillis() - startTime).toString)

		}
		
		fut onFailure {
			case failed : Exception => println("Failed=>" + failed.toString)
		}
		
		result
	}
}



object MyFuture extends App {
	 implicit val actorSystem = ActorSystem("future")
	 
	 val powerToN = new PowerToN(2500, 4, 3)
	 powerToN.compositeFuture

	 Thread.sleep(20000)
	 actorSystem.shutdown
	 println("Main.completed")
}