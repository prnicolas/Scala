/**
 * Classes that illustrates the composition of Futures in Scala 2.10.2 and Akka 2.14 framework, using the for-comprehensive loop monad.
 * Thread.sleep is used to emulate a lengthy computation
 * 
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


class PowerToN(val x : Int, val n : Int = 2) {
        final val sleepDuration : Int = 1000
        implicit val timeout = Timeout(sleepDuration<<1)

        def compositeFuture : Int = {
              var result : Int = -1
                  // First future
              val sumN = Future[Int]  {
                   Thread.sleep(sleepDuration)
                   (1 until n).foldLeft(0)( _ * _)
               }
                  // Second future
               val expN = Future[Int] {
                    Thread.sleep(sleepDuration + 150)
                    Math.exp(x*n).ceil.toInt
               }
                 //3rd future
               val divN = Future[Int] {
                   Thread.sleep(sleepDuration>>1)
                    x / n
              }
                // Nomadic composition of the 3 futures, sum, exp and divide
                // the execution of the summation is blocked until all three
                // futures completed execution
              val summation = for {
                   z <- sumN;
                   y <- expN
                   t <- divN
              } yield z+y+t
  
                  // Call back for successful execution of the future, summation
             summation onSuccess { case value : Int => { result = value } }
    
                  // Call back for failed execution of the future, summation
              summation onFailure {
                   case failed : ArithmeticException => println("Failed=>" + failed.toString)  
              }
             result
       }
}


// ------------------------  EOF -----------------------------------------------------------------------------