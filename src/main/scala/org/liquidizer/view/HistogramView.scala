package org.liquidizer.view

import scala.xml._

import net.liftweb._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.model._
import org.liquidizer.lib._

object HistogramView {
  val cache = new ResultCache[Box[XmlResponse]]

  def getOptions() : Map[String,String] = {
    Map(
      "grid" -> S.param("grid").getOrElse("on"),
      "width" -> S.param("width").getOrElse("640"),
      "height" -> S.param("height").getOrElse("400"),
    "dx" -> S.param("dx").getOrElse("0.1")
    )
  }

  def hist(queryId : String) : Box[LiftResponse] = {
    val query= Query.get(queryId)
    hist(query.get)
  }

  def hist(query : Query) : Box[LiftResponse] = {
    val options= getOptions()
    cache.get(S.uri, options, () => {
      val dx = options.get("dx").get.toDouble
      val data = getData(query, dx)
      val node= (new GnuplotAPI).hist(data.reverse, options)
      Full(XmlResponse(node, "image/svg+xml"))
    })
  }

  def getData(query : Query, dx : Double): List[(Double,Double)] = {

    var histMap= Map[Int, Double]()
    
    VoteMap
    .getAllVoters(VotableQuery(query))
    .foreach { user => 
      val w= VoteMap.getCurrentWeight(user)
      if (w>5e-3) {
	val v= VoteMap.getWeight(user, VotableQuery(query))
	if (v.abs > 5e-3) {      
	  val x= Math.round((1-1e-10)*(v/w/dx-0.5)).toInt
	  histMap+= x -> (histMap.get(x).getOrElse(0.)+w)
	}
      }
    }
 
    histMap
    .map { case (x,y) => ((x+0.5)*dx, y) }
    .toList
    .sort { case (a,b) => a._1 < b._1 }
  }
}
