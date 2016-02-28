/**
 * Classes that illustrates the composition of Futures in Scala 2.10.2 and Akka 2.14 framework, using the for-comprehensive loop monad.
 * Thread.sleep is used to emulate a lengthy computation
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * Changes made on EE
 * @author Patrick Nicolas
 * @date July 23, 2013
 * @see http://patricknicolas.blogspot.com
 */
package algorithms

import akka.actor.{Actor, ActorSystem, ActorRef, Props}
import akka.util.Timeout
import akka.pattern.ask
import scala.concurrent.{Await, Future, Promise}
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global


class CompositeFutures(val x : Int, val n : Int = 2) {
   final val sleepDuration: Int = 1000
   implicit val timeout = Timeout(sleepDuration<<1)

   def compositeFuture: Int = {
                  // First future
      val sumN = Future[Int]  {
          Thread.sleep(sleepDuration)
          (1 until n).foldLeft(1)( _ * _)
      }
                  // Second future
      val expN = Future[Int] {
          println("1")
          Thread.sleep(sleepDuration + 150)
          println("2")
          Math.exp(x*n).ceil.toInt
      }
                 //3rd future
      val divN = Future[Int] {
         Thread.sleep(sleepDuration>>1)
         x / n
      }
                
      	// Monadic composition of the 3 futures, sum, exp and divide
      	// the execution of the summation is blocked until all three
      	// futures completed execution
      val summation = for {
         z <- sumN;
         y <- expN
         t <- divN
      } yield z+y+t
  
      val result: Int = Await.result(summation, timeout.duration).asInstanceOf[Int]  
      
                        // Call back for failed execution of the future, summation

      
      summation onFailure {
          case failed: ArithmeticException => println(failed.toString)
          case _ => println("error")
      }
      
      result
   }
}


// ------------------------  EOF -----------------------------------------------------------------------------