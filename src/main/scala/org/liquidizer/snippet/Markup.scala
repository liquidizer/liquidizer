package org.liquidizer.snippet

import scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.common._

import _root_.org.liquidizer.model._
import _root_.org.liquidizer.lib.Tick

object Markup {

  val url= "(http:/[^\\s\"]*[^\\s!?&.<>])".r
  val nl= "(\\s*(\n|\r)+\\s*)+".r
 
  def renderComment(in : String) : NodeSeq = {
    if (in==null || in.length==0) return Nil
    val node:Node= Text(in)
    var text= node.toString
    
    text= url.replaceAllIn(text, "<a href=\"$1\" class=\"extern\">$1</a>")
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

  def renderHeader(in : String, link : String) : NodeSeq = 
    renderHeader(in, { node => <a href={link}>{node}</a> })

  def renderHeader(in : String, link : Node=>Node) : NodeSeq = {
    url.findFirstMatchIn(in) match {
      case Some(m) =>
        link(Text(m.before.toString)) ++ 
        <a href={m.matched} title={m.matched} class="extern">LINK</a> ++ 
        renderHeader(m.after.toString, link)
      case _ =>
	link(Text(in))
    }
  }

  def formatRelTime(time : Long) : String = {
    val dt= Tick.now - time
    if (dt <= Tick.min) (dt/Tick.sec) + " Sekunden"
    else if (dt <= Tick.h) (dt/Tick.min) + " Minuten"
    else if (dt < 2*Tick.h) 1 + " Stunde"
    else if (dt <= Tick.day) (dt/Tick.h) + " Stunden"
    else if (dt < 2*Tick.day) 1 + " Tag"
    else (dt/Tick.day) + " Tagen"
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
	// determine target and current link
	var keep= attribs.get("keep").map { _.text }
	var href= attribs.get("href").map { _.text }.getOrElse(S.uri).
	    replaceAll("~", User.currentUser.map { _.id.is }.getOrElse(1).toString)

        var active= href == (if (href.startsWith("/")) S.uri else S.uri.replaceAll(".*/",""))

        // set up parameters
        attribs.get("action").foreach { action =>
	  val field= action.text
	  val value= attribs.get("value").get.text
	  active &&= value == S.param(field).getOrElse(
	    if (attribs.get("default").map{ _.text=="true" }.getOrElse(false)) value else "")
	  href += "?"+field+"="+value
	  if (keep.isEmpty)
	    keep=Some("search")
	}
        // add persisted parameters
        keep.foreach { _.split(" ").foreach { key => S.param(key).foreach { value =>
	  if (value.length>0) href += ("&" + key + "=" + value) }}}

        // format link as either active (currently visited) or inaktive
	if (active)
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
