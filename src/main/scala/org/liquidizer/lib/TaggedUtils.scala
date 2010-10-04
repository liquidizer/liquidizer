package org.liquidizer.lib

import scala.collection.mutable
import org.liquidizer.model._

object TaggedUtils {

  def getTags(keys : String) : List[String] = {
    if (keys==null) Nil else
    keys.split("(\\s|,)").map { _.trim }.toList.filter { !_.isEmpty }
  }

  def sortedTags(queries : List[Query]): List[String] = {
    val map = mutable.Map.empty[String, Double]
    var weight= queries.size+1
    for (query <- queries) {
      val keys= getTags(query.keys.is)
      for (key <- keys) {
	map.put(key, map.get(key).getOrElse(0.0) + 
		(weight / (queries.size+1.0)) / keys.size)
      }
      weight-= 1;
    }
    
    map
    .toList
    .sort { case ((k0, v0),(k1,v1)) => v0>v1 }
    .map { case (key, value) => key }
  }

  def sortedUserTags(users : List[User]): List[String] = {
  
    val map = mutable.Map.empty[String, Double]
    var weight= users.size+1
    for (user <- users) {
      val keys= getTags(user.profile.is).filter { _ startsWith "#" }
      for (key <- keys) {
	map.put(key, map.get(key).getOrElse(0.0) + 
		(weight / (users.size+1.0)) / keys.size)
      }
      weight-= 1;
    }
    
    map
    .toList
    .sort { case ((k0, v0),(k1,v1)) => v0>v1 }
    .map { case (key, value) => key }
  }
}
