package org.liquidizer.lib

import scala.collection.mutable
import org.liquidizer.model._

object TaggedUtils {

  def sortedTags(queries : List[Query]): List[String] = {
  
    val map = mutable.Map.empty[String, Double]
    
    var weight= queries.size+1
    queries.foreach {
      query => {
	val keys= query.keyList
	keys.foreach {
	  key => {
	    map.put(key, map.get(key).getOrElse(0.0) + 
		    (weight / (queries.size+1.0)) / keys.size)
	  }
	}
	weight-= 1;
      }}
    
    map
    .toList
    .sort { case ((k0, v0),(k1,v1)) => v0>v1 }
    .map { case (key, value) => key }
	
  }      
}
