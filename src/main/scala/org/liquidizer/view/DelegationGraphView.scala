package org.liquidizer.view
import scala.xml._

import net.liftweb._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._
import Helpers._

import org.liquidizer.model._

class DelegationGraphView(node : Votable, count : Int) {
  val grapher= new GraphvizAPI(node)
  grapher.build(node, count)

  def getGraph : Node = {
    val svgNode= grapher.runGraphviz.first
    val svg= addLink(svgNode)
    val width= svg.attribute("width").get.text.replace("pt","").toInt
    val height= svg.attribute("height").get.text.replace("pt","").toInt
    val scale= Math.min(600.0/width, 600.0/height)
    if (scale < 1) {
      SVGUtil.resize(svg, (width*scale).toInt, (height*scale).toInt)
    }
    else svg
  }

  private def addLink(in : NodeSeq) : NodeSeq = {
    for( node <- in ) yield addLink(node)
  }
  
  def addLink(in: Node) : Node = {
    in match {

      case Elem(prefix, "g", attribs, scope, children @ _*)
      if (attribs.get("class").map{ _.text }.getOrElse("")=="node") =>
	<a xlink:href={ (in \ "title").text.replace("index","graph")} target="_parent">{
	  Elem(prefix, "g", attribs, scope, addLink(children) : _*)
	}</a>

      
      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, attribs, scope, addLink(children) : _*)

      case other => other
    }
  }
  
  def getQueries = grapher.nodes.filter { _ isQuery }
}

object DelegationGraphView {

  val cache = new ResultCache[Node]

  def graphSVG(node : Votable, count:Int) : Node = {
    new DelegationGraphView(node, count).getGraph
  }

  def queryGraph(poll : String) : Box[LiftResponse] = {
    val count= S.param("nodes").getOrElse("10").toInt
    val nominee= Votable.find(By(Votable.query, poll.toLong)).get
    val node= graphSVG(nominee, count)
    Full(XmlResponse(node, "image/svg+xml"))
  }

  def userGraph(userid : String) : Box[LiftResponse] = {
    val count= S.param("nodes").getOrElse("10").toInt
    val nominee= Votable.find(By(Votable.user, userid.toLong)).get
    val node= graphSVG(nominee, count)
    Full(XmlResponse(node, "image/svg+xml"))
  }

}
