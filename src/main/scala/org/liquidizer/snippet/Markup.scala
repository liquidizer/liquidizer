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

  val URL1= "(https?:/[^\\s\"]*[^\\s!?&.<>])"
  val URL2= "\\["+URL1+" ([^]]*)\\]"
  val URL_R= (URL1+"|"+URL2).r
 
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
      case List(line, _*) if line.matches(" *[*-] .*") => {
	val li = <li>{ renderLine(line.replaceAll("^ *[*-]","")) }</li>
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
    <span class="keys">{
      var isFirst= true
      in.map { key =>
	val suff= if (isFirst) NodeSeq.Empty else Text(", ")
        isFirst= false
	suff ++ <a class="tag" href={Helpers.appendParams("/queries.html",("search",key) :: Nil)}>{key}</a>
    }}</span>
  }

  def renderHeader(in : String, link : String) : NodeSeq =
    renderHeader(in, { node => <a href={link}>{node}</a> })

  def renderHeader(in : String, link : Node=>Node) : NodeSeq = {
    var c=0
    renderHeader(in, link, x=> {c=c+1; "["+c+"]"})
  }

  /** render a header replacing links with a generic short cut */
  def renderHeader(in : String, link : Node=>Node, short : String=>String) : NodeSeq = {
    URL_R.findFirstMatchIn(in) match {
      case Some(m) =>
        link(Text(m.before.toString)) ++ {
          if (m.group(1)==null) {
	    eLink(m.group(2), "["+m.group(3)+"]")
	  } else {
	    eLink(m.matched, short(m.matched))
	  }
	} ++
        renderHeader(m.after.toString, link, short)
      case _ =>
	link(Text(in))
    }
  }

  /** make an external link */
  def eLink(url : String, display : String) =
    <a href={url} title={url} target="_blank" class="extern">{display}</a>
}

class CategoryView(val keys : List[String], rootLink:String)  {

  def this(keyStr : String, rootLink : String) = 
    this(keyStr.toLowerCase.split(",| ").distinct.toList, rootLink)

  def isActive(tag : String) = keys.contains(tag.toLowerCase)

  def link(node : Node, keys : List[String]) = {
    val sort= S.param("sort").map { v => List(("sort", v)) }.getOrElse(Nil)
    val uri= Helpers.appendParams(rootLink, ("search" -> keys.mkString(" ")) :: sort)
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

/** Create menu item elements with links and automatic styles */
class MenuMarker extends InRoom {
  val user= User.currentUser.map { _.id.is }.getOrElse(0L)
  var isFirst= true

  def toUrl(url : Option[Seq[Node]]) =
    uri(url.map { _.text }.getOrElse(S.uri))

  def bind(in :NodeSeq) : NodeSeq = in.flatMap(bind(_))
  def bind(in :Node) : NodeSeq = {
    in match {
      case Elem("menu", "a", attribs, scope, children @ _*) =>
	// determine target and current link
	var href= toUrl(attribs.get("href"))
	var active= 
	   if (href.startsWith("/"))
	    S.uri.replaceAll("^/room/[^/]*","") == href.replaceAll("^/room/[^/]+","")
	   else
	    S.uri.replaceAll(".*/","") == href

        // set up parameters
        attribs.get("action").foreach { action =>
	  val value= attribs.get("value").get.text
	  val field= action.text
	  // set the default order if action is sorting
	  if (isFirst && field=="sort")
	    S.set("defaultOrder", value)
	  // Link leads to the currently viewed page
	  active &&= S.param(field).map { value == _ }.getOrElse(isFirst)
	  val search= S.param("search").map { v => List(("search",v))}.getOrElse(Nil)
	  href = Helpers.appendParams(href, (field, value) :: search)
	  isFirst= false
        }

        // make menu entry
	val entry= attribs.get("icon").map { url =>
	  <img src={"/images/menu/"+url.text+".png"} alt="" class="menu"/>
	}.getOrElse(NodeSeq.Empty) ++ Localizer.loc(children)

        // format link as either active (currently visited) or inaktive
        val style= if (active) "active" else "inactive" 
        val title= attribs.get("title").map( Localizer.loc(_) )
        <li><a href={href} title={title} alt={title}><div class={style}>{entry}</div></a></li>

      case Elem("local", "a", attribs, scope, children @ _*) =>
	// keep a number of current request attributes in the target url
        val keep= attribs.get("keep").map{_.text}.getOrElse("")
        val para= S.param(keep).map { v => List((keep,v))}.getOrElse(Nil)
        val url= Helpers.appendParams(toUrl(attribs.get("href")), para )
        val title= attribs.get("title").map( Localizer.loc(_) )
        <a href={url} alt={title} title={title}>{ children }</a>

      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, Localizer.loc(attribs), scope, bind(children) : _*)

      case _ => in
    }
  }

  def render(in : NodeSeq) : NodeSeq = {
    bind(in)
  }
}
