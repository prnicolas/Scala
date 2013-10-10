// Copyright (C) 2010-2012 Patrick Nicolas
package algorithms.kmeans


class Centroid[T <: Double](val vals: Array[T]) extends DataPoint[T](vals, -1) {

    def this(dataPoint: DataPoint[T]) = this(dataPoint.values)
    
    def += (dataPoint: DataPoint[T]) : Unit = {
         var result = (values.size == dataPoint.values.size ) 
         if (result ) {
              var k = 0;
              values.map{ x => { x + dataPoint.values.apply(k); k += 1}}
         }
         result
    }
    
    def /= (numdata: Int) : Unit = {
        values.map{ x => x / numdata }
    }
  
	override def toString : String = {
		val buf = new StringBuilder
		val lastDataIndex = values.size-1;
		var k = 0
			
		while( k < lastDataIndex) {
			  buf.append(values.apply(k))  
			  buf.append(",")
		}
		buf.append(values.apply(lastDataIndex))
		buf.append("\n")
		buf.toString
	}
}

			/**
			 * <p>Implements a cluster of data point with centroid for the KMeans
			 * algorithm. A cluster is composed of a centroid, a list of data point and 
			 * an index.</p>
			 * @author Patrick Nicolas
			 * @date 12.19.2011
			 */

import scala.collection.mutable.ArrayBuffer
class KMeansCluster[T <: Double](val index: Int, 
                                 val values: Array[T],
                                 val distance: (DataPoint[T], DataPoint[T]) => Double) {
	
    val data = new ArrayBuffer[DataPoint[T]]
    val centroid = new Centroid[T](values)
    
    def this(index: Int, 
    		dataPoint: DataPoint[T], 
    		distance: (DataPoint[T], DataPoint[T]) => Double)  = this(index, dataPoint.values, distance)
  
    @inline
    def size : Int = data.size


		/**
		 * <p>Compute the x and y coordinate of the centroid of a cluster.</p>
		 * @param cluster cluster for which the centroid is to be computed.
		 */
	private def updateCentroid : Unit = {
		
		var k = 0
		while( k < data.size ) {
           centroid += data.apply(k)
           k += 1		
		}
		centroid /= data.size
	}
	
		/**
		 * <p>Access the centroid of this cluster.</p>
		 * @return cluster centroid
		 */
	def computeCentroid : Double = {
	    updateCentroid
	    
	    var k = 0
	    while( k < data.size) {
	        distance( data.apply(k), centroid)
	        k += 1
	    }
		computeSumOfSquares
	}

	    
		/**
		 * <p>Add or attach a new data point to this cluster.</p>
		 * @param point to be added to this cluster
		 */
	def += (dataPoint : DataPoint[T]) : Double = { 
	//	val dist = distance( dataPoint, centroid)
		data += dataPoint
		computeSumOfSquares
	}
	

			/**
			 * <p>Detach or remove a new data point from this cluster.</p>
			 * @param point to be removed from this cluster
			 */
	def -=  (dataPoint : DataPoint[T]) : Double = {
		data -= dataPoint
		computeSumOfSquares
	}

		/**
		 * <p>Compute the sum of the squares for all the data points.
		 */
	def computeSumOfSquares : Double = { 
	    var sumSquares : Double = 0.0
	    var i = 0
	    while( i < data.size) {
	        sumSquares += distance(data.apply(i), centroid)
	        i += 1
	    }
	    sumSquares
	}

	def getName : String = {
	    new StringBuilder("Cluster_1").
	             append(index).toString
	}

	

	def printCentroid : String = {
		new StringBuilder(getName).
		        append(",").
		           append(centroid.toString).toString
	}
	
		/**
		 * <p>Textual representation of a cluster.<p>
		 */
	/*
	@Override def toString : String = { 
	    
		new StringBuilder(getName);
		buf.append(" ");
		buf.append(_centroid.toString());
		buf.append(" SumSquares=");
		buf.append(_sumSquares);
		
		for(CDataPoint point : _dataPointsList) {
			buf.append("\n");
			buf.append(point.toString());
		}
		
		return buf.toString();		
	}
	* 
	*/
}

// --------------------------  EOF ---------------------------------