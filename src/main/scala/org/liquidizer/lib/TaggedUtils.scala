package org.liquidizer.lib

import net.liftweb.mapper._
import scala.collection.mutable
import org.liquidizer.model._

object TaggedUtils {

  def getTags(keys : String) : List[String] = {
    if (keys==null) Nil else
    keys.split("(\\s|,)").map { _.trim }.toList.filter { !_.isEmpty }
  }

  def sortedQueryTags() = sortedTags( Votable.findAll(By_>(Votable.query,0)) )

  def sortedTags(data : List[Votable]): List[String] = {
    val names = mutable.Map.empty[String, String]
    val map = mutable.Map.empty[String, Double]
    for (item <- data) {
      val keys= item match {
	case VotableQuery(query) => getTags(query.keys.is)
	case VotableUser(user) => getTags(user.profile.is)
      }
      val weight= VoteMap.getCurrentResult(item).volume
      for (okey <- keys) {
	val key= okey.toLowerCase
	if (!names.contains(key)) names.put(key, okey)
	map.put(key, map.get(key).getOrElse(0.0) + (weight / Math.sqrt(keys.size)))
      }
    }
    
    map
    .toList
    .sort { case ((k0, v0),(k1,v1)) => v0>v1 }
    .map { case (key, value) => names.get(key).get }
  }
}
