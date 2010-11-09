package org.liquidizer.view
import scala.xml._

import net.liftweb._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._
import Helpers._

import org.liquidizer.model._

object DelegationGraphView {

  val cache = new ResultCache[Node]

  private def addLink(in : NodeSeq) : NodeSeq = {
    for( node <- in ) yield addLink(node)
  }
  
  def addLink(in: Node) : Node = {
    in match {

      case Elem(prefix, "g", attribs, scope, children @ _*)
      if (attribs.get("class").map{ _.text }.getOrElse("")=="node") =>
	<a xlink:href={ (in \ "title").text+"/graph.html"} target="_parent">{
	  Elem(prefix, "g", attribs, scope, addLink(children) : _*)
	}</a>

      
      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, attribs, scope, addLink(children) : _*)

      case other => other
    }
  }
  
  def graphSVG(node : Votable, count:Int) : Node = {
    cache.get(node.getClass.toString + node.id+"/"+count, Map(), () => {
      val grapher= new GraphvizAPI
      val svgNode= grapher.toGraph(node, count).first
      val svg= addLink(svgNode)
      val width= svg.attribute("width").get.text.replace("pt","").toInt
      val height= svg.attribute("height").get.text.replace("pt","").toInt
      val scale= Math.min(600.0/width, 600.0/height)
      if (scale < 1) {
	SVGUtil.resize(svg, (width*scale).toInt, (height*scale).toInt)
      }
	else svg
    })
  }

  def queryGraph(poll : String) : Box[LiftResponse] = {
    val count= S.param("nodes").getOrElse("10").toInt
    val query= Query.getQuery(poll).get
    val node= graphSVG(VotableQuery(query), count)
    Full(XmlResponse(node, "image/svg+xml"))
  }

  def userGraph(userid : String) : Box[LiftResponse] = {
    val count= S.param("nodes").getOrElse("10").toInt
    val user= User.getUser(userid).get
    val node= graphSVG(VotableUser(user), count)
    Full(XmlResponse(node, "image/svg+xml"))
  }

}
