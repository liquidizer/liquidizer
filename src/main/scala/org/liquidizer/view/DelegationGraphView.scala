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
  
  def superGraph() : Box[LiftResponse] = {
    val grapher= new GraphvizAPI
    val node= grapher.toGraph().first
    Full(XmlResponse(addLink(node), "image/svg+xml"))
  }

  def graphNode(user : User) : Node = {
    cache.get("user"+user.id.is, Map(), () => {
      val grapher= new GraphvizAPI
      val node= grapher.toGraph(user).first
      addLink(node)
    })
  }

  def graphNode(query : Query) : Node = {
    cache.get("query"+query.id.is, Map(), () => {
      val grapher= new GraphvizAPI
      val node=grapher.toGraph(VotableQuery(query)).first
      addLink(node)
    })
  }

  def queryGraph(poll : String) : Box[LiftResponse] = {
    val query= Query.getQuery(poll).get
    val node= graphNode(query)
    Full(XmlResponse(node, "image/svg+xml"))
  }

  def userGraph(userid : String) : Box[LiftResponse] = {
    val user= User.getUser(userid).get
    val node= graphNode(user)
    Full(XmlResponse(node, "image/svg+xml"))
  }

}
