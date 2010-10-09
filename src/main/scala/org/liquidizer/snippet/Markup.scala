package org.liquidizer.snippet

import scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.common._


object Markup {

  val url= "(http:/[^\\s]*[^\\s!?&.<>])".r
  val nl= "(\\s*(\n|\r)+\\s*)+".r
 
  def renderComment(in:String) : NodeSeq = {
    if (in==null || in.length==0) return Nil
    val node:Node= Text(in)
    var text= node.toString

    text= url.replaceAllIn(text, "<a href=\"$1\">$1</a>")
    text= nl.replaceAllIn(text, "<br/>")

    try {
      val src=scala.io.Source.fromString("<div>"+text+"</div>")
      scala.xml.parsing.XhtmlParser.apply(src)
    } catch {
      case _ => Text(in)
    }
  }
  
  def renderTagList(in:List[String]) : Node = {
    <span class="keys">{in.mkString(", ")}</span>
  }
}

class CategoryView(val keys : List[String], rootLink:String) {

  def link(node : Node, keys: List[String]) = {
    val uri= rootLink+"?search="+keys.mkString(" ")
    <a href={uri}>{node}</a>
  }

  def renderTag(tag : String) : Node = {
    if (keys.contains(tag.toLowerCase)) {
      link(<div class="active">{tag}</div>, keys.filter{ _ != tag.toLowerCase })
    } else {
      link(<div class="inactive">{tag}</div>, tag :: keys)
    }
  }

  def renderTagList(in:List[String]) : NodeSeq = {
    renderTagList(in, in.size+1)
  }

  def renderTagList(in : List[String], len : Int) : NodeSeq = {
    renderTagList(in, len, "tagList")
  }

  def renderTagList(in : List[String], len : Int, id: String) : NodeSeq = {
    <span>{in.slice(0, len).map { tag => renderTag(tag) }}</span>
    <span id={id}>{
      if (len < in.size)
	SHtml.a(() => SetHtml(id, 
	         renderTagList(in.drop(len).toList, len, id+"x")),
		<div class="inactive">...</div>)
		   
      else 
	NodeSeq.Empty
    }</span>
  }
}

class MenuMarker {

  def bind(in :NodeSeq) : NodeSeq = {
    in.flatMap(bind(_))
  }

  def bind(in :Node) : NodeSeq = {
    in match {
      case Elem("menu", "a", attribs, scope, children @ _*) =>
	val href= attribs.get("href").map { _.text }.getOrElse("")
        val cur= if (href.startsWith("/")) 
	  S.uri
        else
	  S.uri.replaceAll(".*/","")
	if (href==cur)
	  <div class="active">{children}</div>
        else
	  <a href={href}><div class="inactive">{children}</div></a>

      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, attribs, scope, bind(children) : _*)

      case _ => in
    }
  }

  def render(in : NodeSeq) : NodeSeq = {
    bind(in)
  }
}
