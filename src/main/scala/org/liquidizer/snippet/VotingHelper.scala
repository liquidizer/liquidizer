package org.liquidizer.snippet

import scala.xml._

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._
import Helpers._

import _root_.org.liquidizer.lib._
import _root_.org.liquidizer.model._
import _root_.org.liquidizer.view.DelegationGraphView
import _root_.org.liquidizer.view.EmotionView

class VotingHelper {
  val currentUser= User.currentUser
  var no= 0

  val buttonFactory = new EditButtonToggler
  
  var displayedVotes = List[(()=>Node, Node)]()
  
  def renderVote(result : () => Node):Node = {
    val node= result()
    displayedVotes ::= (result, node)
    <span id={"dynamicvote"+displayedVotes.size}>{node}</span>
  }
  def formatDouble(value : Double) : String =  String.format("%3.2f",double2Double(value+1e-3))

  def formatResult(value : Double, style: String) : Node = 
    <span class={style}> { formatDouble(value) } </span>

  def formatResult(value : Double) : Node =
      formatResult(value, 
		   if (value>=5e-3) "pro" else if (value< (-5e-3)) "contra" else "pass")

  def formatTrend(trend : Double) : Node =
    <img src={ "/images/" + { 
      if (trend.abs < 1) "flat" else if (trend>0) "up" else "down" }+".png" 
	    } alt=""/>

  def formatResult(nominee : Votable, showTrend : Boolean) : Node = {
    val r= VoteCounter.getResult(nominee)
    if (showTrend)
      <span> { 
	val trend= VoteCounter.getSwing(nominee)
	formatResult(r.value) ++ formatTrend(trend)
      } </span>
    else 
      formatResult(r.value)
  }

  def formatWeight(user: User, nominee : Votable, format:String):Node = {
    format match {
      case "decimal" => 
	formatResult(VoteCounter.getWeight(user,nominee))
      case _ => 
	val weight= VoteCounter.getPreference(user, nominee)
	val style= if (weight>0) "pro" else if (weight<0) "contra" else "pass" 
	<span class={style}>{
	  if (weight>0) { "+" + weight } else weight.toString }</span>
    }
  }

  def formatUser(user : User) : NodeSeq = formatNominee(VotableUser(user))
  def formatNominee(nominee : Votable) : NodeSeq = 
    Markup.renderHeader(nominee.toString, nominee.uri+"/index.html")
  
  /** Update the voting preferences */
  def vote(nominee : Votable, newVote : Int) : JsCmd = {
    PollingBooth.vote(currentUser.get, nominee, newVote)
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

  /** bind the input html segment to a given nominee, either a query or a user */
  def bind(in : Node, nominee:Votable) : NodeSeq = {
    in match {

      // The poll name space for tags that apply for all kind of nominees
      case Elem("poll", tag, attribs, scope, children @ _*) => tag match {
	case "name" => formatNominee(nominee)
	case "id" => Text(nominee.id.toString)
	case "no" => no+=1; Text(no.toString)
	case "title" => Text(nominee.toString)
	case "result" =>
 	  val showTrend= attribs.get("trend").map { _.text=="show" }.getOrElse( false )
	  renderVote(() => formatResult(nominee, showTrend))
	case "value" =>
	  renderVote(() => Text(formatDouble(
	    attribs.get("measure").map { _.text }.getOrElse("") match {
	      case "pro" => VoteCounter.getResult(nominee).pro
	      case "contra" => VoteCounter.getResult(nominee).contra
	      case "volume" => VoteCounter.getResult(nominee).pro
	      case _ => VoteCounter.getResult(nominee).value
	    })))
	case "chart" => chart(nominee.uri, "chart", in.attributes)
	case "hist" => chart(nominee.uri, "histogram", in.attributes)
	case "supporters" =>
	  getSupporters().flatMap {
	    user => bind(bind(children, user, nominee), VotableUser(user))
	  }
	case "aboutLastComment" => 
	  VoteCounter.getLatestComment(nominee) match {
	    case Some(comment) => 
	      Text(S.?("time.commented")+" "+ 
		   Markup.formatRelTime(comment.date.is)+" (") ++
		   formatUser(comment.getAuthor) ++ Text(")")
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
	    case "weight" =>
	      renderVote(() => {
		val format= attribs.get("format").getOrElse(Text("numer"))
		formatWeight(me, nominee, format.text)} )
	    case "vote" =>
	      val isUser= nominee.isUser
	      new VoteControl(VoteCounter.getPreference(me, nominee),
			      displayedVotes.size, 
			      if (isUser) 0 else -3, 3) {
		override def updateValue(newValue : Int) : JsCmd = 
		  super.updateValue(newValue) & vote(nominee, newValue) }
	      .render(children)

	    case "editButton" =>
	      buttonFactory.toggleButton
	    case "isDelegated" => if (nominee match {
	      case VotableUser(other) => VoteCounter.isDelegated(other, me)
	      case VotableQuery(q) => VoteCounter.isDelegated(q.creator.obj.get, me)})
	      bind(children,nominee) else NodeSeq.Empty
	    case _ => in
	  }
	  case _ => NodeSeq.Empty
	}

      // The query namespace only applies to query nominees
      case Elem("query", tag, attribs, scope, _*) =>
	nominee match {
	  case VotableQuery(query) => tag match { 
	    case "creator" => formatNominee(VotableUser(query.creator.obj.get))
	    case "time" => Text(Tick.format(query.creation.is))
	    case "keys" => 
	      buttonFactory.newKeyListRecord(() => query.keyList,
					     list => query.keys(list).save )
	      buttonFactory.toggleText
	    case "numVoters" => 
	      val sign= attribs.get("weight").get.text.toInt
	      renderVote(() => 
		Text(VoteCounter
		     .getAllVoters(query)
		     .filter{ sign*VoteCounter.getWeight(_, nominee)>0 } 
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
	    case "inflow" =>  
	      <a href={nominee.uri+"/support.html"}>{
		renderVote(() => formatResult(VoteCounter.getDelegationInflow(user)))
	      }</a>
	    case "outflow" =>  
	      <a href={nominee.uri+"/delegates.html"}>{
		renderVote(() => {
		  val outflow= VoteCounter.getDelegationOutflow(user)
		  formatResult(outflow, if (outflow>5e-3) "contra" else "pass")
		})
	      }</a>
	    case "itsme" => if (Full(user)==currentUser) bind(children, nominee) else NodeSeq.Empty
	    case "notme" => if (Full(user)!=currentUser) bind(children, nominee) else NodeSeq.Empty
	    case "votes" => getVotes().flatMap { vote => bind(bind(children, user, vote), vote) }
	    case "delegates" => getDelegates().flatMap { 
	      delegate => bind(bind(children, user, VotableUser(delegate)), VotableUser(delegate))
	    }
	    case "emoticon" => 
		renderVote(() => EmotionView.emoticon(user, attribs))
	    case _ => in
	  }
	  case _ => tag match {
	    case "itsme" => NodeSeq.Empty
	    case "notme" => bind(children, nominee)
	    case _ => in
	  }
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
	    () => VoteCounter.getCommentText(user, nominee),
	    text => PollingBooth.comment(user, nominee, text))
	  buttonFactory.toggleText ++
	  // add comment time
	  VoteCounter.getComment(user, nominee).map { c => 
	    <span class="keys">{ Markup.formatRelTime( c.date.is) }</span> }
	  .getOrElse(Nil)
	}
	case _ => Elem("user", label, attribs, scope, rec(true) : _*)
      }

      case Elem("poll", label, attribs, scope, _*) => label match {
	case "name" => formatNominee(nominee)
	case "notme" => rec(currentUser.isEmpty || VotableUser(currentUser.get)!=nominee)
	case "itsme" => rec(!currentUser.isEmpty && VotableUser(currentUser.get)==nominee) 
	case _ =>  Elem("poll", label, attribs, scope, rec(true) : _*)

      }

      case Elem("vote", label, attribs, scope, _*) =>
	label match {
	  case "result"  => {
	    // format voting weights
	    val format= in.attribute("format").getOrElse(Text("decimal"))
	    renderVote(() => formatWeight(user,nominee,format.text))
	  }
	  case "weight" => {
	    // format delegation weights
	    val style= in.attribute("style").getOrElse(Text("pro"))
	    renderVote(() => formatResult(
	      VoteCounter.getDelegationInflow(user) * 
	      VoteCounter.getCumulativeWeight(user, nominee), style.text))
	  }
	}

      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, attribs, scope, rec(true) : _*)

      case other => other
    }
  }

  def getVotes() : List[Votable] = {
    throw new Exception("No votes for this item")
  }

  def getDelegates() : List[User] = {
    throw new Exception("No delegates for this item")
  }

  def getSupporters() : List[User] = {
    throw new Exception("No supporters for this item")
  }
}
