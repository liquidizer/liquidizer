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

/** Gets the room id and provides room related convenience functions */
trait InRoom {
  /** Prefix to indicate that rooms are hidden from the links */
  val SINGLE_ROOM_PRFIX= "_"

  val showRoomInUrl= S.param("room").exists ( !_.startsWith(SINGLE_ROOM_PRFIX) )
  val roomName= S.param("room").getOrElse("").replaceAll("^"+SINGLE_ROOM_PRFIX+"?","")
  lazy val room= Room.get(roomName)

  /** searches the votable for user in the current room */
  def toNominee(user : User) : Option[Votable] =
    Votable.find(By(Votable.user, user), By(Votable.room, room.get))

  /** searches the current users votable */
  lazy val myNominee= if (User.currentUser.isEmpty) None else
    toNominee(User.currentUser.get)

  /** formats the URL prefix for the current room */
  def home() = if (showRoomInUrl) "/room/"+roomName else ""
  def uri(user : User) = home() + "/users/" + user.id.is + "/index.html"
  def uri(query : Query) = home() + "/queries/" + query.id.is + "/index.html"
  def uri(nominee : Votable) : String = nominee match {
    case VotableUser(user) => uri(user)
    case VotableQuery(query) => uri(query)
  }

  /** formats the URL by replacing # and ~ place holders */
  def uri(rel : String) = {
    var u= rel.replaceAll("^#", home())
    if (u.contains("~"))
      u= u.replaceAll("~", User.currentUser.get.id.is.toString)
    if (!showRoomInUrl)
      u= u.replaceAll("^/room/[^/]+","")
    u
  }
}

/** Base class for searched and sorted multi-page views of votables */
abstract class MultipageSnippet extends StatefulSnippet with InRoom {
  var dispatch : DispatchIt = { 
    case "render" => render _ 
    case "navigator" => navigator _
    case "lastpage" => lastpage _
    case "categories" => categories _
    case "search" => search _
  }

  def size= data.size
  var data : List[Votable] = Nil
  val tags= new TaggedUtil()

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
  trait MultiPageHelper extends VotingHelper {
    no= from
    override def slice(list : List[Votable]) = list.slice(from, to)
  }

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
	nominee => isUser(nominee, u => bonus(u) + superSorter(nominee))
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
	case VotableUser(u) if VoteMap.isActive(u, q.room.obj.get)=> f(u) 
	case _ => -1e10 }
    def withMe(f : (User, Votable) => Double) : Votable => Double =
      nominee => User.currentUser match {
	case Full(me) => f(me, nominee) + 1e-5*result(nominee).value
	case _ => 0 }
    def isActive(f : Votable => Double) : Votable => Double = 
      q => if (result(q).volume==0) -1e5 else f(q)

    order match {
      case "value" => isActive(result(_).value.abs)
      case "age" => q => q.id.is
      case "pro" => isActive(result(_).value)
      case "contra" => isActive(-result(_).value)
      case "conflict" => isActive(v => { val r=result(v); r.pro min r.contra })
      case "myweight" => withMe(VoteMap.getWeight(_ , _))
      case "swing" => isActive(q => (VoteMap.getSwing(q)/(1+result(q).value.abs)).abs)
      case "volume" => isActive(result(_).volume)
      case "comment" => 
	Comment.getLatest(_).map{ _.date.is.toDouble }.getOrElse(0.0)
      case "valence" => 
	withMe((me,v) => isUser(v, VoteMap.getSympathy(me, _, v.room.obj.get)))
      case "avalence" =>
	withMe((me,v) => isUser(v,-VoteMap.getSympathy(me, _,  v.room.obj.get)))
      case "arousal" =>
	withMe((me,v) => isUser(v,VoteMap.getArousal(me, _,  v.room.obj.get)))
    }
  }

  def defaultOrder= S.get("defaultOrder").getOrElse("value")
  def sortData(): Unit = 
    sortData(() => sortFunction(order.getOrElse(defaultOrder)))
  def sortData(nominee: Votable) : Unit = 
    sortData(() => sortFunction(order.getOrElse(defaultOrder), nominee))
  def sortData(user : User): Unit = 
    sortData(() => sortFunction(order.getOrElse(defaultOrder), user))

  /** Sort data according to the sort order in request parameter */
  def sortData(f : () => Votable => Double): Unit = {
    if (order.exists(_=="alpha")) {
      data = data.sort { _.toString < _.toString }
    } else {
      val f_ = f()
      data = data
      .map { item => (f_(item), item) }
      .sort { (a,b) =>  
	if (a._1 > b._1) 
	  true
	else 
	  if (a._1 < b._1) 
	    false
	  else a._2.id > b._2.id }
      .map { _._2 }
    }
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
	(if (page+SPAN < numPages-2) 
	  link(-1,Text("...")) else NodeSeq.Empty)++
	(if (page+SPAN < numPages-1)
	  link(numPages-1, Text(numPages.toString)) else NodeSeq.Empty) ++
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

  def lastpage(in:NodeSeq) : NodeSeq = if (page>=numPages-1) in else Nil

  /** Format a link with parameters that persists current search and sort */
  def attribUri(params : (String,String)*) = {
    uri(Helpers.appendParams(S.uri, {
      order.map { o => Map("sort" -> o) }.getOrElse(Map()) ++
      Map("search" -> search) ++ Map(params:_*) }.toList))
  }

  def search(in : NodeSeq) : NodeSeq = {
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
  }

  def searchFilter(text : String) : Boolean = {
    val keys= search.split(" +").map { _.toUpperCase }
    var pass= true;
    for (key <- keys) pass &&= text.toUpperCase.contains(key)
    pass
  }

  def searchFilter(nominee : Votable) : Boolean = {
    val keys= search.split(" +").map { _.toUpperCase }
    var pass= true;
    for (key <- keys) {
      pass &&=
	nominee.toString.toUpperCase.contains(key) || (
	  key.startsWith("#") &&
	  tags.keyList(nominee).map{ _.toUpperCase }.contains(key))
    }
    pass
  }
}
