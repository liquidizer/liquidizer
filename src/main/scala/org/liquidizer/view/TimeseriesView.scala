package org.liquidizer.view

import scala.xml._

import net.liftweb._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._
import Helpers._

import org.liquidizer.model._
import org.liquidizer.lib._

object TimeseriesView {

  val cache = new ResultCache[Box[XmlResponse]]

  private def addLink(in : NodeSeq, ref : String) : NodeSeq = {
    for( node <- in ) yield addLink(node, ref)
  }
  
  def addLink(in: Node, ref : String) : Node = {
    in match {

      case Elem(prefix, "svg", attribs, scope, children @ _*) =>
	val opts= options()
      Elem(prefix, "svg", attribs, scope, 
	   <a xlink:href={ref} target="_parent">
	   <rect x="0" y="0" 
	   width={opts.get("width").get} 
	   height={opts.get("height").get}  fill="white"/>{
	     addLink(children, ref)
	   }</a>
	 )

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
	Elem(prefix, label, attribs, scope, addLink(children, ref) : _*)

      case other => other
    }
  }

  def createResponse(node: Node) = {
    Full(XmlResponse(addLink(node,S.uri.replaceAll("chart.svg","analyzer.html")), "image/svg+xml"))
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
    User.getUser(userId) match {
      case Some(user) => {
	cache.get(S.uri, options, () => {
	  val ts= QuoteHistory.getTimeSeries(VotableUser(user))
	  val node= (new GnuplotAPI).plotTS(ts, options, false, true, false)
	  createResponse(node.first)
	})
      }
      case _ => Empty
    }
  }
  
  def queryChart(queryId : String) : Box[LiftResponse] = {
    Query.getQuery(queryId) match {
      case Some(query) => {
	cache.get(S.uri, options, () => {
	  println("QueryChart requested ...")
	  val ts= QuoteHistory.getTimeSeries(VotableQuery(query))
	  println("TimeSeries=" +ts)
	  val node= (new GnuplotAPI).plotTS(ts, options, true, true, true)
	  createResponse(node.first)
	})
      }
      case _ => Empty
    }
  }
  
  def voteQueryChart(userId : String, queryId : String) : Box[LiftResponse] = {
    val query= Query.getQuery(queryId)
    val user= User.getUser(userId)
    if (query.isEmpty || user.isEmpty)
      Empty
    else
      cache.get(S.uri, options, () => {
	val ts= QuoteHistory.getTimeSeries((user.get,VotableQuery(query.get)))
	val node= (new GnuplotAPI).plotTS(ts, options, false, true, true)
	createResponse(node.first)
      })
  }

  def voteUserChart(userId : String, delegateId : String) : Box[LiftResponse] = {
    val user= User.getUser(userId)
    val user2= User.getUser(delegateId)
    if (user.isEmpty || user2.isEmpty)
      Empty 
    else
      cache.get(S.uri, options, () => {
	val ts= QuoteHistory.getTimeSeries((user.get,VotableUser(user2.get)))
	val node= (new GnuplotAPI).plotTS(ts, options, false, true, false)
	createResponse(node.first)
    })
  }
}
