package org.liquidizer.snippet

import scala.xml._
import scala.xml.parsing._

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._

import org.liquidizer.model._
import org.liquidizer.lib.Tick

object Markup {

  val url= "(http:/[^\\s\"]*[^\\s!?&.<>])".r
 
  def renderComment(in : String) : NodeSeq = {
    if (in==null || in.length==0)
      NodeSeq.Empty
    else
      toXHTML(in)
  }

  def toXHTML(input : String) : NodeSeq = {
    try {
      val src= scala.io.Source.fromString("<span>"+input+"</span>")
      tidy(XhtmlParser(src).first.child, false)
    } catch {
      case e:FatalError => <p>{input}</p>
    }
  }
  
  def tidy(seq : NodeSeq, isLink : Boolean) : NodeSeq = 
    seq.flatMap { tidy(_, isLink) }

  def tidy(node : Node, isLink : Boolean) : NodeSeq = node match {
    case Elem(ns, tag, attr, scope, ch @ _*) =>
      tag match {
	case "img" | "code" | "p"  =>
	  val allowed= Set("src", "width", "height")
	  val fAttr= attr.filter { n=> allowed.contains(n.key) }
	  Elem(ns, tag, fAttr, scope, tidy(ch, true) :_*)
	case _ => Text(node.toString)
      }
    case Text(text) if !isLink => renderBlock(text.split("\n").toList)
    case _ => Text(node.toString)
  }

  def renderBlock(in : List[String]) : List[Node] = {
    def tail= renderBlock(in.tail)
    in match {
      case Nil => Nil
      case List(line, _*) if line.matches(" *[*#].*") => {
	val li = <li>{ renderLine(line.replaceAll("^ *[*#]","")) }</li>
	tail match {
	  case <ul>{ c @ _* }</ul> :: t => <ul>{ li ++ c }</ul> :: t
	  case t => <ul>{ li }</ul> :: t
	}
      }
      case List(line, _*) => <p>{ renderLine(line) }</p> :: tail
    }
  }

  def renderLine(in : String) = renderHeader(in, x=>x)

  def renderTagList(in:List[String]) : Node = {
    <span class="keys">{in.mkString(", ")}</span>
  }

  def renderHeader(in : String, link : String) : NodeSeq = 
    renderHeader(in, { node => <a href={link}>{node}</a> })

  /** render a header replacing links with a generic short cut */
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

  /** format a time relative to now */
  def formatRelTime(time : Long) : String = "vor " + {
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

  def this(keyStr : String, rootLink : String) = 
    this(keyStr.toLowerCase.split(",| ").distinct.toList, rootLink)

  def isActive(tag : String) = keys.contains(tag.toLowerCase)

  def link(node : Node, keys : List[String]) = {
    val uri= rootLink+"?search="+keys.mkString(" ")
    <a href={uri}>{node}</a>
  }

  def getStyle(isactive : Boolean) = if (isactive) "active" else "inactive"
  def getKeysWith(tag : String) = tag :: keys
  def getKeysWithout(tag : String) = keys.filter{ _ != tag.toLowerCase }

  def renderTag(tag : String) : Node = {
    if (isActive(tag)) {
      link(<div class={getStyle(true)}>{tag}</div>, getKeysWithout(tag))
    } else {
      link(<div class={getStyle(false)}>{tag}</div>, getKeysWith(tag))
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
	val isDefault= attribs.get("default").map{ _.text=="true" }.getOrElse(false)

        // set up parameters
        attribs.get("action").foreach { action =>
	  val value= attribs.get("value").get.text
	  val field= action.text
	  if (isDefault && field=="sort") S.set("defaultOrder", value)
	  active &&= S.param(field).map { value == _ }.getOrElse(isDefault)
	  href += "?"+field+"="+value
	  if (keep.isEmpty)
	    keep=Some("search")
	}
        // add persisted parameters
        keep.foreach { _.split(" ").foreach { key => S.param(key).foreach { value =>
	  if (value.length>0) href += ("&" + key + "=" + value) }}}

        // make icon
	val icon= attribs.get("icon").map { url =>
	  <img src={"/images/menu/"+url.text+".png"} alt="" class="menu"/>
	}.getOrElse(NodeSeq.Empty)

        // format link as either active (currently visited) or inaktive
	if (active)
	  <div class="active">{icon ++ children}</div>
        else
	  <a href={href}><div class="inactive">{icon ++ children}</div></a>

      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, attribs, scope, bind(children) : _*)

      case _ => in
    }
  }

  def render(in : NodeSeq) : NodeSeq = {
    bind(in)
  }
}
