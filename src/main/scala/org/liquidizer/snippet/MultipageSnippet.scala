package org.liquidizer.snippet

import scala.xml.{Node,NodeSeq,Elem,Text}
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.mapper._
import Helpers._

import org.liquidizer.model._
import org.liquidizer.lib._

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

  val defaultsize= 12
  val pagesize:Int = S.param("pagesize").map{ _.toInt }.getOrElse(defaultsize)
  var search= S.param("search").getOrElse("")
  var order= S.param("sort")
  
  def numPages= (size+pagesize-1)/pagesize
  def page:Int = { S.param("page").map{ _.toInt }.getOrElse(0) }
  
  def from = pagesize * page
  def to = Math.min(pagesize * (page+1), size)

  /** Create a sort ordering function for users voting on a fixed nominee */
  def sortFunction(order: String, nominee:Votable) : (Votable => Double) = {
    def weight(user:User) = VoteMap.getWeight(user, nominee)
    def comment(user:User) = Comment.get(user, nominee)
    def bonus(user:User) = 
      if (comment(user).isEmpty && Full(user)!=User.currentUser) 0 else 1000
    def isUser(q:Votable, f:(User=>Double)) = 
      q match { case VotableUser(u) => f(u) + bonus(u) case _ => -1e10 }

    order match {
      case "weight" => isUser(_, user => weight(user).abs)
      case "approval" => isUser(_, weight(_))
      case "objection" => isUser(_, -weight(_))
      case "comment_age" => isUser(_, Comment.getTime(_, nominee))
      case _ => 
	val superSorter= sortFunction(order)
	isUser(_, u => bonus(u) + superSorter(VotableUser(u)))
    }
  }

  /** Create a sort ordering function for votes cast by a fixed user */
  def sortFunction(order: String, user:User) : (Votable => Double) = {
    def weight(nominee : Votable) = VoteMap.getWeight(user, nominee)
    order match {
      case "weight" => weight(_).abs
      case "approval" => weight(_)
      case "objection" => -weight(_)
      case "comment_age" => Comment.getTime(user, _)
      case _ => sortFunction(order)
    }
  }

  /** Create a sort ordering function for users and queries */
  def sortFunction(order : String) : (Votable => Double) = {
    def result(q:Votable)= VoteMap.getCurrentResult(q)
    def isUser(q:Votable, f:(User=>Double)) = 
      q match { 
	case VotableUser(u) if VoteMap.isActive(u)=> f(u) 
	case _ => -1e10 }
    def withMe(f : (User, Votable) => Double) : Votable => Double =
      nominee => User.currentUser match {
	case Full(me) => f(me, nominee) + 1e-5*result(nominee).value
	case _ => 0 }
    def isActive(f : Votable => Double) : Votable => Double = 
      q => if (result(q).volume==0) -1e5 else f(q)

    order match {
      case "value" => isActive(result(_).value.abs)
      case "age" => q => q.baseId
      case "pro" => isActive(result(_).value)
      case "contra" => isActive(-result(_).value)
      case "conflict" => isActive(v => { val r=result(v); r.pro min r.contra })
      case "myweight" => withMe(VoteMap.getWeight(_ , _))
      case "swing" => isActive(q => (VoteMap.getSwing(q)/(1+result(q).value.abs)).abs)
      case "volume" => isActive(result(_).volume)
      case "comment" => 
	Comment.getLatest(_).map{ _.date.is.toDouble }.getOrElse(0.0)
      case "valence" => 
	withMe((me,v) => isUser(v,VoteMap.getSympathy(me,_)))
      case "avalence" =>
	withMe((me,v) => isUser(v,- VoteMap.getSympathy(me,_)))
      case "arousal" =>
	withMe((me,v) => isUser(v,VoteMap.getArousal(me,_)))
    }
  }

  def defaultOrder= S.get("defaultOrder").getOrElse("value")
  def sortData(): Unit = 
    sortData(sortFunction(order.getOrElse(defaultOrder)))
  def sortData(nominee: Votable) : Unit = 
    sortData(sortFunction(order.getOrElse(defaultOrder), nominee))
  def sortData(user : User): Unit = 
    sortData(sortFunction(order.getOrElse(defaultOrder), user))

  /** Sort data according to the sort order in request parameter */
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
  
  /** Display a navigation bar to access each page directly */
  def navigator(in: NodeSeq) : NodeSeq = {
    if (numPages<=1)
      <span/>
    else {
      val SPAN= 5 
      <div class="navi"> {
	link(page-1, Text("<<<")) ++
	link(0, Text("1")) ++
	(if (page-SPAN > 1) 
	  link(-1,Text("...")) else NodeSeq.Empty) ++
	(2 max (page-SPAN+1) to numPages.min(page+SPAN+1)).flatMap {
	  index => link(index-1, Text(index.toString)) } ++
	(if (page+SPAN < numPages-1) 
	  link(-1,Text("..."))++link(numPages-1, Text(numPages.toString)) 
	 else NodeSeq.Empty) ++
	link(page+1, Text(">>>"))
      } </div>
    }
  }
  
  private def link(targetPage:Int, text:Node):Node = {
    if (targetPage==page)
      <div class="active">{text}</div>
    else if (targetPage<0 || targetPage >= numPages)
      <div>{text}</div>
    else 
	link("" , () => {
	  redirectTo(attribUri("page" -> targetPage.toString))}, 
	     <div>{text}</div>)
  }

  def lastpage(in:NodeSeq) : NodeSeq = if (page>=numPages-1) view(in) else Nil

  def view(in : NodeSeq) : NodeSeq = in.flatMap(view(_))

  def attribUri(params : (String,String)*) = {
    S.uri + {
      (Map("sort" -> order.getOrElse(""), "search" -> search) ++ Map(params:_*))
      .filter { case (key, value) => value.length>0 }
      .map { case (key,value) => key+"="+value }
      .mkString("?","&","")
    }
  }

  def view(in : Node) : NodeSeq = {
    in match {
      case <search:input/> => 
	SHtml.text(search, search = _ , 
		   "width" -> "20", 
		   "placeholder" -> "search", "class" -> "searchText") ++
	SHtml.ajaxSubmit("Search", { () =>
	  this.unregisterThisSnippet
	  S.redirectTo(attribUri("search"-> search)) },
		       "class" -> "searchSubmit") ++
        {
	  // Display a clear search text button if applicable
	  if (search.length==0) Nil else
	    SHtml.ajaxSubmit("X", { () =>
	      this.unregisterThisSnippet
	      S.redirectTo(attribUri("search"->"")) },
				 "class" -> "searchClear")
	}

      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, attribs, scope, view(children) : _*)

      case _ => in
    }
  }

  def searchFilter : String => Boolean = {
    val keys= search.split(" +").map { _.toUpperCase }
    val f= {
      text:String =>
	val uText= if (text==null) "" else text.toUpperCase
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
