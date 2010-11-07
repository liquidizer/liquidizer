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
    case "addQuery" => addQuery _
  }

  def size= data.size
  var data : List[Votable] = Nil

  def render(in:NodeSeq) : NodeSeq
  def categories(in:NodeSeq) : NodeSeq = NodeSeq.Empty

  val defaultsize= 20
  val pagesize:Int = S.param("pagesize").map{ _.toInt }.getOrElse(defaultsize)
  var search= S.param("search").getOrElse("")
  var order= S.param("sort").getOrElse("")
  
  def numPages= (size+pagesize-1)/pagesize
  def page:Int = { S.param("page").map{ _.toInt }.getOrElse(0) }
  
  def from = pagesize * page
  def to = Math.min(pagesize * (page+1), size)

  /** Create a sort ordering function for users voting on a fixed nominee */
  def sortFunction(order: String, nominee:Votable) : (Votable => Double) = {
    def inflow(user:User) = VoteCounter.getDelegationInflow(user)
    def weight(user:User) = VoteCounter.getWeight(user, nominee)
    def comment(user:User) = VoteCounter.getComment(user, nominee)
    def bonus(user:User) = if (comment(user).isEmpty) 0 else 1000
    def isUser(q:Votable, f:(User=>Double)) = 
      q match { case VotableUser(u) => f(u) + bonus(u) case _ => -1e10 }

    order match {
      case "" | "weight" => isUser(_, user => inflow(user)*weight(user).abs)
      case "approval" => isUser(_, weight(_))
      case "objection" => isUser(_, -weight(_))
      case "comment_age" => isUser(_, VoteCounter.getCommentTime(_, nominee))
      case _ => 
	val superSorter= sortFunction(order)
	isUser(_, u => bonus(u) + superSorter(VotableUser(u)))
    }
  }

  /** Create a sort ordering function for votes cast by a fixed user */
  def sortFunction(order: String, user:User) : (Votable => Double) = {
    def weight(nominee : Votable) = VoteCounter.getWeight(user, nominee)
    order match {
      case "" | "weight" => weight(_).abs
      case "approval" => weight(_)
      case "objection" => -weight(_)
      case "comment_age" => VoteCounter.getCommentTime(user, _)
      case _ => sortFunction(order)
    }
  }

  /** Create a sort ordering function for users and queries */
  def sortFunction(order : String) : (Votable => Double) = {
    def result(q:Votable)= VoteCounter.getResult(q)
    def isUser(q:Votable, f:(User=>Double)) = 
      q match { case VotableUser(u) => f(u) case _ => -1e10 }
    def emotion(q:Votable, f:(Emotion=>Double)) =
      isUser(q, u=>
	     User.currentUser match { 
	       case Full(me) => VoteCounter.getEmotion(me,u).map{ f(_) }.getOrElse(-1e9)
	       case _=> -1e10 })
    order match {
      case "" | "value" => result(_).value.abs
      case "age" => q => q.id
      case "pro" => result(_).value
      case "contra" => -result(_).value
      case "swing" => VoteCounter.getSwing(_)
      case "volume" => result(_).volume
      case "inflow" => isUser(_, VoteCounter.getDelegationInflow(_))
      case "outflow" => isUser(_, VoteCounter.getDelegationInflow(_))
      case "valence" => emotion(_, _.valence.value)
      case "avalence" => emotion(_, -_.valence.value)
      case "arousal" => emotion(_, _.getArousal)
    }
  }

  def sortData(): Unit = sortData(sortFunction(order))
  def sortData(nominee: Votable): Unit = sortData(sortFunction(order, nominee))
  def sortData(user: User): Unit = sortData(sortFunction(order, user))

  def sortData(f : Votable => Double): Unit = {
    data = data
    .map { item => (f(item), item) }
    .sort { (a,b) =>  
      if (a._1 > b._1) 
	true
      else 
	 if (a._1 < b._1) 
	   false
	 else a._2.id > b._2.id }
    .map { _._2 }
  }
  
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
	SHtml.text(search, search = _ , 
		   "width" -> "15", "placeholder" -> "search")

      case <search:submit/> =>
	SHtml.ajaxSubmit("Search", { () =>
	  this.unregisterThisSnippet
	  S.redirectTo(S.uri+"?search="+search+
		       (if (order.length>0) "&sort="+order else "")) } )

      case <search:clear/> =>
	SHtml.ajaxSubmit("Clear", { () =>
	  this.unregisterThisSnippet
	  S.redirectTo(S.uri + (if (order.length>0) "?sort="+order else "")) } )

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
      keys.isEmpty || keys.filter { key => !uText.contains(key) }.size==0
    }
    f
  }

  def searchFilter(query : Query) : Boolean = {
    searchFilter(query.what.is) || searchFilter(query.keys.is)
  }

  def searchFilter(user : User) : Boolean = {
    searchFilter(user.nick.is) || searchFilter(user.profile.is)
  }

  def searchFilter(nominee : Votable) : Boolean = nominee match {
    case VotableUser(user) => searchFilter(user)
    case VotableQuery(query) => searchFilter(query)
  }

  def addQuery(in: NodeSeq) : NodeSeq = {
    <a href={"/add_query?search="+search}>{in}</a>
  }
}
