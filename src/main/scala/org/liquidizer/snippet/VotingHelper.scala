package org.liquidizer.snippet

import scala.xml.{Node, NodeSeq, Text, Elem, MetaData}

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.mapper._
import Helpers._

import org.liquidizer.lib._
import org.liquidizer.model._
import org.liquidizer.view.DelegationGraphView
import org.liquidizer.view.EmotionView

/** The VotingHelper provides a number of snippet tags for rendering
 *  The details of any votable, e.g. voters, results, names etc.
 */
class VotingHelper extends InRoom {
  lazy val currentUser= User.currentUser
  var no= 0

  val buttonFactory = new EditButtonToggler
  
  var displayedVotes = List[(()=>Node, Node)]()
  
  def renderVote(result : () => Node):Node = {
    val node= result()
    displayedVotes ::= (result, node)
    <span id={"dynamicvote"+displayedVotes.size}>{node}</span>
  }
  def formatDouble(value : Double) : String =  
    String.format("%3.2f",double2Double(value+1e-3))

  def getStyle(value : Double) =
    if (value>=5e-3) "pro" else if (value< (-5e-3)) "contra" else "pass"

  def formatResult(value : String, style: String) : Node = 
    <span class={style}> { value } </span>

  def formatResult(value : Double) : Node =
      formatResult(formatDouble(value), getStyle(value)) 

  def formatPercent(value : Double) : Node =
    formatResult((value*100.9).toInt+"%", getStyle(value))

  def formatTrend(trend : Double) : Node =
    <img src={ "/images/" + { 
      if (trend.abs < 1) "flat" else if (trend>0) "up" else "down" }+".png" 
	    } alt=""/>

  def formatResult(nominee : Votable, showTrend : Boolean) : Node = {
    val r= VoteMap.getCurrentResult(nominee)
    if (showTrend)
      <span> { 
	val trend= 10*VoteMap.getSwing(nominee)/(1.0+r.value.abs)
	formatResult(r.value) ++ formatTrend(trend)
      } </span>
    else 
      formatResult(r.value)
  }

  def formatWeight(user: User, nominee : Votable):Node =
    formatResult(VoteMap.getWeight(user,nominee))

  def formatUser(user : User) : NodeSeq = 
    Markup.renderHeader(user.toString, uri(user))

  def formatNominee(nominee : Votable) : NodeSeq = 
    Markup.renderHeader(nominee.toString, uri(nominee))
  
  /** Update the voting preferences */
  def vote(nominee : Votable, newVote : Int) : JsCmd = {
    PollingBooth.vote(currentUser.get, nominee, newVote)
    VoteMap.refresh()
    ajaxUpdate(nominee)
  }
  
  /** detect which entries in the dynamic votes list changed and update */
  def ajaxUpdate(nominee : Votable) : JsCmd = {
    var index1= displayedVotes.size+1
    val oldVotes= displayedVotes.map { case (r, n) => (r, n, r()) }
    displayedVotes= oldVotes.map { case (r, n1, n2) => (r, n2) }

    oldVotes
    .map {
      case (r, n1, n2) =>
 	index1-= 1
        if (n1.toString == n2.toString)
	  Noop
        else
 	  SetHtml("dynamicvote"+index1, n2)
    }
  }

  /** bind the input html segment to a given nominee, either a query or a user */
  def bind(in : NodeSeq, nominee:Votable) : NodeSeq = {
    in.flatMap(bind(_, nominee))
  }

  /** Bind the input html to a given nominee, either a query or a user */
  def bind(in : Node, nominee:Votable) : NodeSeq = {
    in match {
      // The poll name space for tags that apply for all kind of nominees
      case Elem("poll", tag, attribs, scope, children @ _*) => tag match {
	case "name" => formatNominee(nominee)
	case "id" => Text(nominee.id.toString)
	case "no" => no+=1; Text(no.toString)
	case "title" => Text(nominee.toString)
	case "result" =>
 	  val showTrend= attribs.get("trend").exists { _.text=="show" }
	  renderVote(() => formatResult(nominee, showTrend))
	case "value" =>
	  renderVote(() => Text(formatDouble(
	    attribs.get("measure").map { _.text }.getOrElse("") match {
	      case "pro" => VoteMap.getCurrentResult(nominee).pro
	      case "contra" => VoteMap.getCurrentResult(nominee).contra
	      case "volume" => VoteMap.getCurrentResult(nominee).pro
	      case _ => VoteMap.getCurrentResult(nominee).value
	    })))
	case "chart" => chart(uri(nominee), "chart", in.attributes)
        case "hist" => chart(uri(nominee), "histogram", in.attributes)
	case "aboutLastComment" => 
	  Comment.getLatest(nominee) match {
	    case Some(comment) => 
	      Text(S.?("time.commented")+" "+ 
		   Markup.formatRelTime(comment.date.is)+" (") ++
		   comment.author.obj.map { formatUser(_) }
		   .openOr { Text("-") } ++ Text(")")
	    case _ => Nil }

	case "graph-nodes" => 
	  val nodes= attribs.get("count").get.text.toInt
	  <a href={S.uri+"?nodes="+nodes}>{children}</a>
	case "graph" => 
	  val nodes= S.param("nodes").getOrElse("10").toInt
	  val grapher= new DelegationGraphView(nominee, nodes)
	  grapher.getGraph ++
	  grapher.getQueries.flatMap { bind( children, _ ) }
      }
      
      // The me namespace for tags that apply to the current user
      case Elem("me", tag, attribs, scope, children @ _*) =>
	currentUser match {
	  case Full(me) => tag match {
	    case "weight" => renderVote(() => formatWeight(me, nominee))
	    case "vote" =>
	      val isUser= nominee.isUser
	      new VoteControl(VoteMap.getPreference(me, nominee),
			      displayedVotes.size, 
			      if (isUser) 0 else -3, 3) {
		override def updateValue(newValue : Int) : JsCmd = 
		  super.updateValue(newValue) & vote(nominee, newValue) }
	      .render(children)
	    case "editButton" =>
	      buttonFactory.toggleButton
	    //TODO democratic tagging system
	    case "isDelegated" => if (nominee match {
	      case VotableUser(other) => 
		myNominee.exists { VoteMap.isDelegated(other, _) }
	      case VotableQuery(q) => 
		myNominee.exists { VoteMap.isDelegated(q.creator.obj.get, _) }})
	      bind(children,nominee) else NodeSeq.Empty
	    case _ => in
	  }
	  case _ => NodeSeq.Empty
	}

      // The query namespace only applies to query nominees
      case Elem("query", tag, attribs, scope, _*) =>
	nominee match {
	  case VotableQuery(query) => tag match { 
	    case "creator" => formatUser(query.creator.obj.get)
	    case "time" => Text(Tick.format(query.creation.is))
	    case "keys" => 
	      buttonFactory.newKeyListRecord(() => query.keyList,
					     list => query.keys(list).save )
	      buttonFactory.toggleText
	    case "numVoters" => 
	      val sign= attribs.get("weight").get.text.toInt
	      renderVote(() => 
		Text(VoteMap
		     .getAllVoters(nominee)
		     .filter{ sign*VoteMap.getWeight(_, nominee)>0 } 
		     .size.toString))
	    case _ => in
	  }
	  case _ => in
	}

      // The user name space for users as nominees
      case Elem("user", tag, attribs, scope, children @ _*) =>
	nominee match {
	  case VotableUser(user) => tag match {
	    case "weightof" =>
	      val str= (S?"user.weightof").split("\\$")
	      Text(str.head)++formatNominee(nominee)++Text(str.last)
	    case "profile" => 
	      val maxlen= attribs.get("maxlen").map { _.text.toInt }.getOrElse(0)
	      buttonFactory.newCommentRecord(() => user.profile.is, 
					     value => { user.profile(value); user.save }, maxlen)
	      buttonFactory.toggleText
	    case "sympathy" =>  
	      renderVote(() => formatPercent(
		currentUser.map {
		VoteMap.getSympathy(_, user, room.get) }.getOrElse(0.0)))
	    case "popularity" =>  
	      renderVote(() => formatPercent(VoteMap.getCurrentResult(nominee).value))
	    case "itsme" => if (Full(user)==currentUser) bind(children, nominee) else NodeSeq.Empty
	    case "notme" => if (Full(user)!=currentUser) bind(children, nominee) else NodeSeq.Empty
	    case "emoticon" => 
		renderVote(() => EmotionView.emoticon(nominee, attribs))
	    case _ => in
	  }
	  case _ => tag match {
	    case "itsme" => NodeSeq.Empty
	    case "notme" => bind(children, nominee)
	    case _ => in
	  }
	}
      
      // sub lists of dependent entries
      case Elem("data", tag, attribs, scope, children @ _*) => 
	val src= attribs.get("src").map { _.text }.getOrElse(tag)
        val data= getData(src)
        tag match {
	  case "size" => Text(data.size.toString)
	  case "votes"|"delegates" => slice(data).flatMap { 
	    item => bind(bind(children, nominee.user.obj.get, item), item) }
	  case "supporters" => slice(data).flatMap {
	    item => bind(bind(children, item.user.obj.get, nominee), item) }
	}

      // Default treatment of unknown tags
      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, attribs, scope, bind(children, nominee) : _*)

      case _ => Localizer.loc(in)
    }
  }

  /** render a timeseries chart */
  def chart(uri:String, chartType:String, attribs:MetaData ) : Node = {
    val options = attribs.elements.map ( a=> a.key+"="+a.value).mkString("&")
    <embed
    alt="Chart"
    src={uri+"/"+chartType+".svg?"+options}
    width={attribs.get("width").getOrElse(Text("640"))}
    height={attribs.get("height").getOrElse(Text("480"))}/>
  }

  def bind(in : NodeSeq, user:User, nominee:Votable) : NodeSeq = {
    in.flatMap(bind(_, user, nominee))
  }

  /** bind the input html fragment to a given user and a given nominee */
  def bind(in : Node, user : User, nominee : Votable): NodeSeq= {

    // conditional recursive processing
    def rec(cond:Boolean) = 
      if (cond) bind(in.child, user, nominee) else NodeSeq.Empty

    in match {
      case Elem("user", label, attribs, scope, _*) => label match {
	case "name" => formatUser(user)
	case "notme" => rec(currentUser!=Full(user))
	case "itsme" => rec(currentUser==Full(user))
	case "comment" => {
	  // editable comment text
	  buttonFactory.newCommentRecord(
	    () => Comment.getText(user, nominee),
	    text => PollingBooth.comment(user, nominee, text))
	  buttonFactory.toggleText ++
	  // add comment time
	  Comment.get(user, nominee).map { c => 
	    <span class="keys">{ Markup.formatRelTime( c.date.is) }</span> }
	  .getOrElse(Nil)
	}
	case _ => Elem("user", label, attribs, scope, rec(true) : _*)
      }

      case Elem("poll", label, attribs, scope, _*) => label match {
	case "name" => formatNominee(nominee)
	case "notme" => rec(currentUser.isEmpty || !nominee.is(currentUser.get))
	case "itsme" => rec(!currentUser.isEmpty && nominee.is(currentUser.get))
	case _ =>  Elem("poll", label, attribs, scope, rec(true) : _*)
      }

      case Elem("vote", label, attribs, scope, _*) =>
	label match {
	  case "result"  =>
	    renderVote(() => formatWeight(user,nominee))
	  case "sympathy" => renderVote(() => {nominee match {
	    case VotableUser(other) =>
	      formatPercent(VoteMap.getSympathy(user, other, room.get))
	    case _ => Text("0.0")
	  }})
	  case "delegation" => 
	    // format the delegation path
	    if (VoteMap.getPreference(user,nominee)!=0 || 
		VoteMap.getWeight(user,nominee).abs<5e-3) NodeSeq.Empty else 
		  <div class="path">{ S ? "vote.delegated.by" }<ul>{
		  for (path <- getDelegationPath(user, nominee)) yield {
		    <li>{ path.reverse.tail.flatMap { 
		      sec => {Text(" → ") ++ formatUser(sec) } } 
		    }</li>
		  }
		}</ul></div>
	}

      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, attribs, scope, rec(true) : _*)

      case other => other
    }
  }

  /** Returns the closest delegation path of a user to a votable */
  def getDelegationPath(user : User, nominee : Votable) : List[List[User]] = {
    var visited : List[User]= Nil
    var result : List[List[User]]= List(Nil)
    var list : List[List[User]]= List(List(user))
    var finished = false
    while (!finished && !list.isEmpty) {
      result= list.filter { p => VoteMap.getPreference( p.head, nominee)!=0 }
      if (result.isEmpty) {
	list= list.flatMap { 
	  path => VoteMap.getActiveVotes(path.head, room.get).map { _ match {
	    case VotableUser(user)
	      if (!visited.contains(user) && VoteMap.getWeight(user, nominee)!=0) 
		=> 
		  user :: path
	    case _ => Nil
	  }}.filter { !_.isEmpty }
        }
	visited= visited ++ list.map { _.head }
      } else {
	finished= true
      }
    }
    result
  }

  /** returns a list of votes as defined by the overriding class */
  def getData(src : String) : List[Votable] = Nil

  /** Select the items of the current view */
  def slice(data : List[Votable]) : List[Votable] = data
}
