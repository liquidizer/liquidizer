package org.liquidizer.snippet

import scala.xml._
import scala.xml.parsing._

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._

import org.liquidizer.model._

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
	case "img" | "em" | "i"  =>
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

  def renderLine(in : String) = renderHeader(in, x=>x, x=>x)

  def renderTagList(in:List[String]) : Node = {
    <span class="keys">{in.mkString(", ")}</span>
  }

  def renderHeader(in : String, link : String) : NodeSeq =
    renderHeader(in, { node => <a href={link}>{node}</a> })

  def renderHeader(in : String, link : Node=>Node) : NodeSeq = {
    var c=0
    renderHeader(in, link, x=> {c=c+1; "["+c+"]"})
  }

  /** render a header replacing links with a generic short cut */
  def renderHeader(in : String, link : Node=>Node, short : String=>String) : NodeSeq = {
    url.findFirstMatchIn(in) match {
      case Some(m) =>
        link(Text(m.before.toString)) ++ 
        <a href={m.matched} title={m.matched} class="extern">{short(m.matched)}</a> ++ 
        renderHeader(m.after.toString, link, short)
      case _ =>
	link(Text(in))
    }
  }

  /** format a time relative to now */
  def formatRelTime(time : Long) : String = {
    val dt= Tick.now - time
    if (dt <= Tick.min) S.?("time.secs").format(dt/Tick.sec)
    else if (dt <= Tick.h) S.?("time.mins").format(dt/Tick.min)
    else if (dt < 2*Tick.h) S.?("time.one.hour")
    else if (dt <= Tick.day) S.?("time.hours").format(dt/Tick.h)
    else if (dt < 2*Tick.day) S.?("time.one.day")
    else S.?("time.days").format(dt/Tick.day)
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

/** The Localizer provides localization for Text nodes
 *  and attributes starting with $ */
object Localizer {
  def loc(in : NodeSeq) : NodeSeq = in.flatMap(loc(_))
  def loc(in : Node) : NodeSeq = in match {
    case Text(str) if (str.startsWith("$")) => Text(loc(str))
    case _ => in
  }
  def loc(in : String) = if (in.startsWith("$")) S.?(in.substring(1)) else in

  /** localize a number of attributes */
  def loc(attr : MetaData) : MetaData = 
    if (attr==Null) Null else attr match {
      case UnprefixedAttribute(key, value, next) =>
	new UnprefixedAttribute(key, loc(value.text), loc(next))
      case _ => throw new Exception("Unknown attribute type : "+attr)
    }	
}

class MenuMarker {

  def bind(in :NodeSeq) : NodeSeq = in.flatMap(bind(_))

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

        // make menu entry
	val entry= attribs.get("icon").map { url =>
	  <img src={"/images/menu/"+url.text+".png"} alt="" class="menu"/>
	}.getOrElse(NodeSeq.Empty) ++ Localizer.loc(children)

        // format link as either active (currently visited) or inaktive
        val style= if (active) "active" else "inactive" 
        <li><a href={href}><div class={style}>{entry}</div></a></li>

      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, Localizer.loc(attribs), scope, bind(children) : _*)

      case _ => in
    }
  }

  def render(in : NodeSeq) : NodeSeq = {
    bind(in)
  }
}
