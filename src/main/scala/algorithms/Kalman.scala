/**
 * @author Patrick Nicolas
 * @date July 19, 2013
 */

package algorithms

import org.apache.commons.math3.filter.KalmanFilter
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.RealVector
import org.apache.commons.math3.filter.DefaultProcessModel
import org.apache.commons.math3.filter.DefaultMeasurementModel
import org.apache.commons.math3.linear.ArrayRealVector
import org.apache.commons.math3.filter.ProcessModel
import org.apache.commons.math3.filter.MeasurementModel
import scala.util.Random


final class KalmanNoise(private val size : Int,
						private val processNoiseGen: () => Double = Random.nextGaussian,
		           		private val measureNoiseGen: () => Double = Random.nextGaussian) {
	
	lazy val processNoise = generate( processNoiseGen )
    lazy val measurementNoise = generate( measureNoiseGen )
    
    private[this] def generate( f: () => Double): ArrayRealVector = {
    	val noise = new ArrayRealVector(size)
		(0 until size) foreach( n => noise.setEntry(n, f()) )
	    noise
    }
}


import KalmanPredictor._
case class KalmanModel(	val A: DblMatrix, 
		     	  		val B: DblMatrix,
		     	  		val H: DblMatrix,
		     	  		val Q: DblMatrix,
		     	  		val R: DblMatrix,
		     	  		val P0: DblMatrix) {
	
   require( A != null && H != null)
   require(A.length == H.length)
   
   @inline
   def processModel(x0 :Array[Double]) : ProcessModel = new DefaultProcessModel(A,B,Q,x0,P0)
   
   @inline
   def measurementModel : MeasurementModel = new DefaultMeasurementModel(H, R)
}
	    


class KalmanPredictor(val model : KalmanModel, val noise : KalmanNoise) {
    require( model != null && noise != null, "Kalman predictor has undefined model and noise")
     
	def compute(u: Array[Double], x0: Array[Double], maxNumIters: Int = 50) : Unit =  {
    	lazy val processModel = model.processModel(x0)
    	lazy val measurementModel = model.measurementModel

    	val filter = new KalmanFilter(processModel, measurementModel)
    	val uVector = new ArrayRealVector(u)
    	var xVector = processModel.getInitialStateEstimate

    
    		// Iterate the sequence: prediction, noise & state estimation and correction
		(0 until maxNumIters).foreach( i => {
			 filter.predict(uVector)
			 xVector = newState(uVector, xVector, processModel)
		     filter.correct(newMeasurement(xVector))
		 })
	}
    
    
		// x = A.x+ B.u + w
    private def newState(uVector : RealVector, xVector : RealVector, processModel : ProcessModel) : RealVector = {
    	val A = processModel.getStateTransitionMatrix
		val B = processModel.getControlMatrix
		val w = noise.processNoise
    	A.operate(xVector).add(B.operate(uVector)).add(w)
    }
    
    	//  z + H.x + v
    private def newMeasurement(xVector : RealVector) : RealVector = {
    	val H = model.measurementModel.getMeasurementMatrix
    	val v = noise.measurementNoise
    	H.operate(xVector).add(v)
    }
}


object KalmanPredictor {
	type DblMatrix = Array[Array[Double]]
}


object KalmanTest extends App {
	object Defaults {
		final val IdentityMatrix = Array(Array[Double](1.0, 1.0), Array[Double](1.0, 1.0))
		final val IdentityVector = Array[Double](1.0, 1.0)
	}
	
	
	val dt = 0.05 
	val initialHeight = 100.0
	val initialSpeed = 0.0
	val processNoise = 0.0
	val measureNoise = 0.2
	val gravity = 9.81
	
	val A = Array(Array[Double](1.0, dt), Array[Double](0.0, 1.0))
	val B = Array(Array[Double](0.5*dt*dt), Array[Double](dt))
	val H = Array(Array[Double](1.0, 0.0))
	val Q = Array(Array[Double](1e-4, 1e-3), Array[Double](1e-3, 1e-4))
	val R = Array(Array[Double](measureNoise*measureNoise))
	
		// Initialize the drop at 100 feet with no speed
	val x0 = Array[Double](initialHeight, initialSpeed)
	
		// Create the process and noise models
	val model = new KalmanModel(A, B, H, null, R, Defaults.IdentityMatrix)
	val noise = new KalmanNoise(2, null, () => Random.nextGaussian*measureNoise)
	
		// Compute the new state for height and velocity
	val predictor = new KalmanPredictor(model, noise)
	predictor.compute(Array[Double](gravity), x0)
}

// -------------------------  EOF -----------------------------------------