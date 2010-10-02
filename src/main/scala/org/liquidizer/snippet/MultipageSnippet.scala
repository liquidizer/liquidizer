package org.liquidizer.snippet

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._
import Helpers._

import _root_.org.liquidizer.model._
import _root_.org.liquidizer.lib._

abstract class MultipageSnippet extends StatefulSnippet {
  var dispatch : DispatchIt = { 
    case "render" => render _ 
    case "navigator" => navigator _
    case "lastpage" => lastpage _
    case "categories" => categories _
    case "view" => view _
  }

  def size:Int
  def render(in:NodeSeq) : NodeSeq
  def categories(in:NodeSeq) : NodeSeq = NodeSeq.Empty

  val pagesize:Int = S.param("pagesize").map{ _.toInt }.getOrElse(10)
  var search= S.param("search").getOrElse("")
  var order= S.param("sort").getOrElse("")
  
  def numPages= (size+pagesize-1)/pagesize
  def page:Int = { S.param("page").map{ _.toInt }.getOrElse(0) }
  
  def from = pagesize * page
  def to = Math.min(pagesize * (page+1), size)
  
  def navigator(in: NodeSeq) : NodeSeq = {
    if (numPages<=1)
      <span/>
    else
      <div class="navi">
      {
	link(page-1, Text("<<<"))
      }
      {
	(1 to numPages).flatMap {
	  index =>
	    link(index-1, Text(index.toString))
	}
      }
      {
	link(page+1, Text(">>>"))
      }
      </div>
  }
  
  private def link(targetPage:Int, text:Node):Node = {
    if (targetPage<0 || targetPage >= numPages || targetPage==page) 
      <div>{text}</div>
    else 
	link("" , () => {
	  redirectTo(S.uri+"?page="+targetPage+
		     (if (search=="") "" else "&search="+search)+
		     (if (order=="") "" else "&sort="+order))}, 
	     <div>{text}</div>)
  }

  def lastpage(in:NodeSeq) : NodeSeq = if (page>=numPages-1) view(in) else Nil

  def view(in : NodeSeq) : NodeSeq = in.flatMap(view(_))

  def view(in : Node) : NodeSeq = {
    in match {
      case <search:input/> => 
	SHtml.text(search, search = _ )

      case <search:submit/> =>
	SHtml.ajaxSubmit("Search", { () =>
	  this.unregisterThisSnippet
	  S.redirectTo(S.uri+"?search="+search+
		       (if (order.length>0) "&sort="+order else "")) } )

      case <search:clear/> =>
	SHtml.ajaxSubmit("Clear", { () =>
	  this.unregisterThisSnippet
	  S.redirectTo(S.uri + (if (order.length>0) "?sort="+order else "")) } )

      case <view:sort>{ _* }</view:sort> => {
	val uri= S.uri
	val options=
	  (in\\"option")
	  .filter { !User.currentUser.isEmpty || 
		   _.attribute("login").map { _.text!="true" }.getOrElse(true) }
	  .map { node => (node.attribute("value").get.text, node.text) }
      
	SHtml.ajaxSelect(options, Full(order), option => {
	  unregisterThisSnippet
	  RedirectTo(uri+"?sort="+option+
		     (if (search.length>0) "&search="+search else "")) })
      }

      case <view:a>{ children @ _* }</view:a> =>
	var attrs= List[String]()
	if (!in.attribute("sort").isEmpty && order.length>0) 
	  attrs ::= "sort="+order
	if (!in.attribute("search").isEmpty && search.length>0)
	  attrs ::= "search="+search
	<a href={ in.attribute("href").get.text +
		 (if (attrs.isEmpty) "" else attrs.mkString("?","&",""))
	       }>{ children }</a>
		 
      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, attribs, scope, view(children) : _*)

      case _ => in
    }
  }

  def clearSearchButton(in : NodeSeq) : NodeSeq = {
    if (search=="")
      NodeSeq.Empty
    else
      <a href={S.uri + (if (order.length>0) "?sort="+order else "")}>{
	in
      }</a>
  }

  def searchFilter : String => Boolean = {
    val keys= search.split(" +").map { _.toUpperCase }
    val f= {
      text:String =>
	val uText= text.toUpperCase
      keys.isEmpty || keys.filter { key => uText.contains(key) }.size>0
    }
    f
  }

  def searchFilter(query : Query) : Boolean = {
    searchFilter(query.what.is) || searchFilter(query.keys.is)
  }

  def searchFilter(user : User) : Boolean = {
    searchFilter(user.nick.is) || searchFilter(user.realname.is)
  }

  def searchFilter(nominee : Votable) : Boolean = nominee match {
    case VotableUser(user) => searchFilter(user)
    case VotableQuery(query) => searchFilter(query)
  }

  
  def voteSortOrder(nominee : Votable) : (User,User)=> Boolean = {
    def volume(u:User)  = {
      Math.abs(VoteCounter.getWeight(u, nominee))
    }
    def swing(u:User)   = VoteCounter.getWeight(u, nominee)
    def comment(u:User) = VoteCounter.getCommentTime(u, nominee)
    def result(u:User)  = VoteCounter.getWeight(u, nominee)
    order match {
      case "pro" => (a,b) => result(a) > result(b)
      case "contra" => (a,b) => result(a) < result(b)
      case "swing" => (a,b) => swing(a) > swing(b)
      case "comment" => (a,b) => comment(a) > comment(b)
      case _ => (a,b) => volume(a) > volume(b)
    }
  }

  def userVolume(user:User, nominee:Votable): Double = {
      Math.abs(VoteCounter.getWeight(user, nominee))
  }

  def voteSortOrder(user:User) : (Votable,Votable)=> Boolean = {
    def volume(v:Votable)  = userVolume(user, v)
    def swing(v:Votable)   = VoteCounter.getWeight(user, v)
    def comment(v:Votable) = VoteCounter.getCommentTime(user, v)
    def result(v:Votable)  = VoteCounter.getWeight(user, v)
    order match {
      case "pro" => (a,b) => result(a) > result(b)
      case "contra" => (a,b) => result(a) < result(b)
      case "swing" => (a,b) => swing(a) > swing(b)
      case "comment" => (a,b) => comment(a) > comment(b)
      case _ => (a,b) => volume(a) > volume(b)
    }
  }
}


