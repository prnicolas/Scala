// Copyright (C) 2010-2012 Patrick Nicolas
package algorithms.kmeans


		/**
		 * <p>Class that implements the K-Means unsupervised learning algorithm. The algorithm
		 * is composed of a list of cluster and initial set of normalized data points.</p>
		 * 
		 * @author Patrick Nicolas
		 * @date 12/11/2011
		 */
final class KMeansClustering[T <: Double](val maxNumIterations: Int, 
                                          val numClusters: Int, 
                                          val convergence: Double,
                                          val distance: (DataPoint[T], DataPoint[T]) => Double,
                                          val initialize:  Array[DataPoint[T]] => Array[DataPoint[T]]) {
    import scala.collection.mutable.ArrayBuffer
        
    val clusters = new ArrayBuffer[KMeansCluster[T]]
 	
		/**
		 * <p>Implements the K-Means unsupervised learning algorithms. The algorithm
		 * iterates to minimize the total sum of squares of distance between each cluster
		 * data points and its centroid.</p>
		 */

	def train(observationsList: Array[DataPoint[T]]) : Int  = {
        require(observationsList != null && observationsList.size > 0,
               "Cannot extract KMeans cluster from undefined data") 
		
               /*
                * Initialize the centroid (pseudo-randomly)
                */
        val centroids = initialize(observationsList)
        		/*
        		 * Create and initialize the clusters
        		 */
		var i = 0
        centroids.foreach( centroid => {
            clusters.append(new KMeansCluster(i, centroid, distance)) 
        })
        
	
		i = 0
		while( i < clusters.size ) {
		    clusters.apply(i) += observationsList.apply(i)
		    i += 1
		    if( i >= observationsList.size) 
		         i = clusters.apply(i).size
		}

		for( cluster <- clusters ) 
			cluster.computeCentroid

			/*
			 * Compute the least sum of squares within the 
			 * number of maximum of iterations.
			 */

		i = 0
		var bestCluster : KMeansCluster[T] = null
		var converged : Boolean = false
		
		while( i < maxNumIterations && !converged) {
		    for( cluster <- clusters) {
		        val newObservations = new ArrayBuffer[DataPoint[T]]
		        for( dataPoint <- cluster.data) 
		              newObservations.append(dataPoint)
		              
		        for( dataPoint <- newObservations ) {
		            var minDistance = Double.MaxValue
		            bestCluster = null
		            
		            for( cluster <- clusters) {
		                val dist = distance(dataPoint, cluster.centroid)
		                if( minDistance > dist) {
		                    minDistance = dist
		                    bestCluster = cluster
		                }
		            }
		            updateDataPoints(dataPoint, cluster, bestCluster) 
		               
		        }  
		    }
		    converged = converge(computeTotalDistance)
		    i += 1
		}
		i
	}
	
	
		/**
		 * <p>Classify the set of observations by extracting the cluster
		 * id containing the set of observations.
		 * @param observation
		 * @return cluster id as a floating point value.
		 */
	def classify(dataPoint: DataPoint[T]) : Int =  {
	  
	    var clusterId = -1
	    var bestScore : Double = 0
	    clusters.foreach( x => { 
	    	val dist = distance(x.centroid, dataPoint)
	    	if( dist < bestScore ) {
	    	     bestScore = dist
	    	     clusterId = x.index
	    	}
	    })
	    clusterId
	}


	
	
	override def toString : String = {
		val buf = new StringBuilder
		for( cluster <- clusters)  {
			buf.append(cluster.printCentroid)
			buf.append("\n")
		}
		buf.append("\n")
	
	    buf.toString
	}
	
	
	
						// ------------------------------
						// Private Supporting Methods
						// ----------------------------
				
	private def converge(totalDistance: Double) : Boolean = totalDistance < convergence
 
	private def computeTotalDistance : Double = {
		var totalDistance = 0.0
		for( cluster <- clusters ) {
			totalDistance += cluster.computeSumOfSquares
		}
		totalDistance
	}

	
	
	@inline
	def dimension : Int = clusters.apply(0).size
	
	private def getParameters(observations: Array[DataPoint[T]]) : Array[Double] = {
		val numVariables : Int = clusters.apply(0).size;
		
		val params = new Array[Double]((numVariables << 1));
		var k = 0
		var j = numVariables
		
		while(k < numVariables)  {
	   	    params.update(k, Double.MinValue)
		    params.update(j, Double.MaxValue)
		    k += 1
		    j += 1
		}

		for( observation <- observations ) {
			k = 0
			j = numVariables
			while( k < numVariables) {
			     if( observation.values.apply(k) > params.apply(k))
			          params.update(k, observation.values.apply(k))
			          
			     if( observation.values.apply(k) < params.apply(j)) 
			          params.update(j, observation.values.apply(k))
			     k += 1
			     j += 1
			}
		}
		
		params;
	}	
	
	
	private def updateDataPoints(observation: DataPoint[T],	
					             cluster : KMeansCluster[T],
						         bestCluster : KMeansCluster[T]) : Boolean = {
	  
		val update = bestCluster != null && bestCluster != cluster;
		
		if( update ) {
			bestCluster += (observation);
			cluster -= (observation);
			
			var i = 0
			while( i < clusters.size)  {
			   clusters.apply(i).computeCentroid
			   i += 1
			}
			converge(computeTotalDistance)
		}
		else
		  update
	}
	
}


object KMeansClustering {
	final val MAX_ITERATIONS 				= 1000;
	final val MIN_NUM_CONVERGING_ITERATIONS = 12;
	final val CONVERGENCE_CRITERIA 			= 0.015F; 
}

// ------------------------------------------  EOF -----------------------------------
