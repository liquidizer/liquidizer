package org.liquidizer.view

import scala.xml._

import net.liftweb._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._
import Helpers._

import org.liquidizer.model._

object TimeseriesView {

  val cache = new ResultCache[Box[XmlResponse]]

  private def reformat(in : NodeSeq) : NodeSeq = {
    for( node <- in ) yield reformat(node)
  }
  
  def reformat(in: Node) : Node = {
    in match {

      case Elem(prefix, "path", attribs, scope, _*) =>
  	if 
  	  (attribs.get("stroke").isEmpty &&
  	   attribs.get("style").map(!_.text.matches("stroke:rgb[^;]*")).getOrElse(true))
  	  in
        else {
  	  Elem(prefix, "path",
  	       attribs
  	       .remove("stroke")
  	       .remove("style")
  	       .append(new UnprefixedAttribute("style", "stroke:#000000;stroke-width:0.5", Null)),
  	       scope)
  	}
         
      case Elem(prefix, label, attribs, scope, children @ _*) =>
  	Elem(prefix, label, attribs, scope, reformat(children) : _*)

      case other => other
    }
  }

  def createResponse(node: Node) = {
    Full(XmlResponse(reformat(node), "image/svg+xml"))
  }
  
  def options() : Map[String,String] = {
    Map(
      "grid" -> S.param("grid").getOrElse("on"),
      "axis" -> S.param("axis").getOrElse("log"),
      "width" -> S.param("width").getOrElse("640"),
      "height" -> S.param("height").getOrElse("400")
    )
  }

  def userChart(userId : String) : Box[LiftResponse] = {
    Votable.find(By(Votable.user, userId.toLong)) match {
      case Full(nominee) => {
	cache.get(S.uri, options, () => {
	  val ts= Tick.getTimeSeries(nominee)
	  val node= (new GnuplotAPI).plotTS(ts, options, false)
	  createResponse(node.first)
	})
      }
      case _ => Empty
    }
  }
  
  def queryChart(queryId : String) : Box[LiftResponse] = {
    Votable.find(By(Votable.query, queryId.toLong)) match {
      case Full(nominee) => {
	cache.get(S.uri, options, () => {
	  val ts= Tick.getTimeSeries(nominee)
	  val node= (new GnuplotAPI).plotTS(ts, options, true)
	  createResponse(node.first)
	})
      }
      case _ => Empty
    }
  }
}
