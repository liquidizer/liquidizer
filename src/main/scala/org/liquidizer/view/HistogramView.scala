package org.liquidizer.view

import scala.xml._

import net.liftweb._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._

import scala.collection.mutable

import org.liquidizer.model._
import org.liquidizer.lib._


class HistogramData(val nominee:Votable, val delegation:Boolean) {

  case class Entry(var weight:Int, 
		   var result:Double, 
		   var isDelegated:Boolean,
		   var primaryVote:Int)
  
  val histMap= mutable.Map.empty[Int, Int]
  val data= mutable.Map.empty[User, Entry]
  val visited= mutable.Set.empty[User]

  extract(nominee, 1.0, 0);

  def extract(nominee: Votable, pathResult:Double, primaryVote0:Int):Unit = {
    VoteCounter.getSupporters(nominee, false)
    .foreach {
      user => {
	val weight= VoteCounter.getWeight(user, nominee)
	val result= pathResult *
	  (if (delegation)
	    weight.toDouble / VoteCounter.getWeight(user)
	   else
	     VoteCounter.getResult(user, nominee))

	var primaryVote= primaryVote0
	if (primaryVote==0)
	  primaryVote = if (weight>0) 1 else -1

	data.get(user) match {
	  case Some(entry) =>
	    
	  entry.result += result
	    if (entry.isDelegated) {
	      if (entry.primaryVote!=primaryVote) {
		if (entry.weight==weight)
		  entry.primaryVote= 0
		else if (entry.weight < weight) {
		  entry.primaryVote= primaryVote
		  entry.weight = weight
	      }}
	    }

	  case _ =>
	    data.put(user, Entry(weight, result, primaryVote0!=0, primaryVote))
	}
	if (delegation && !visited.contains(user)) {
	  visited += user
          extract(VotableUser(user), result, primaryVote)
	}
    }}
  }

  def getData(primaryVote:Int, isDelegated:Boolean, dx:Double):List[(Double,Double)] = {
    data.foreach {
      case (key, entry) =>
	if (entry.isDelegated==isDelegated && entry.primaryVote==primaryVote) {
	  val x= Math.round((1-1e-10)*(entry.result/dx-0.5)).toInt
	  histMap.put(x, histMap.get(x).getOrElse(0)+1)
	}
    }
    histMap
    .map { case (x,y) => ((x+0.5)*dx, y.toDouble) }
    .toList
    .sort { case (a,b) => a._1 < b._1 }
  }
}

object HistogramView {
  val cache = new ResultCache[Box[XmlResponse]]

  def getOptions() : Map[String,String] = {
    Map(
      "grid" -> S.param("grid").getOrElse("on"),
      "width" -> S.param("width").getOrElse("640"),
      "height" -> S.param("height").getOrElse("400"),
      "delegation" -> S.param("delegation").getOrElse("false"),
      "dx" -> S.param("dx").getOrElse("0.1")
    )
  }

  def hist(queryId : String) : Box[LiftResponse] = {
      Query.getQuery(queryId) match {
	case Some(query) => hist(VotableQuery(query))
	case _ => Empty
    }
  }

  def hist(nominee:Votable) : Box[LiftResponse] = {
    val options= getOptions()
    cache.get(S.uri, options, () => {
      val dx = options.get("dx").get.toDouble
      val delegation= options.get("delegation").get=="true"
      val histor= new HistogramData(nominee, delegation)
      val data= List(
	histor.getData(1, false, dx),
	histor.getData(-1, false, dx),
	histor.getData(1, true, dx),
	histor.getData(-1, true, dx),
	histor.getData(0, true, dx))
      val node= (new GnuplotAPI).hist(data.reverse, options)
      Full(XmlResponse(node, "image/svg+xml"))
    })
  }
  
}
