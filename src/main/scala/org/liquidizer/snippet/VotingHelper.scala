package org.liquidizer.snippet

import scala.xml._
import scala.collection.mutable

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

class VotingHelper {
  val currentUser= User.currentUser

  val buttonFactory = new EditButtonToggler
  
  var displayedVotes = List[()=>Node]()
  
  def renderVote(result : () => Node):Node = {
    displayedVotes ::= result
    <span id={"dynamicvote"+displayedVotes.size}>{result()}</span>
  }
  def formatDouble(value : Double) : String =  String.format("%3.2f",double2Double(value))
  def formatResult(value : Double) : Node = {
    <span class={
      if (value>=5e-3) "pro" else if (value< (-5e-3)) "contra" else "pass"
    }>{ formatDouble(value) } </span>	  
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

  def formatNominee(nominee : Votable):Node = 
    <a href={nominee.uri+"/index.html" }>{nominee.toString}</a>
  
  def vote(nominee : Votable, weightChange : Int) : JsCmd = {
    val newVote= VoteCounter.getPreference(currentUser.get, nominee) + weightChange

    // users can not get negative vote
    nominee match {
      case VotableUser(_) => {if (newVote<0) return Noop }
      case _ => {}
    }

    PollingBooth.vote(currentUser.get, nominee, newVote)
    ajaxUpdate(nominee)
  }
  
  def ajaxUpdate(nominee : Votable) : JsCmd = {
    var index1= displayedVotes.size+1
    var index2= displayedEmos.size+1
    displayedVotes.map { 
      displayedVote =>
 	index1-= 1
 	SetHtml("dynamicvote"+index1, displayedVote())
      } ++
    }.toList
  }

  def render(in:NodeSeq, nominee:Votable) : NodeSeq = {
    bind(in, nominee)
  }

  def bind(in : NodeSeq, nominee:Votable) : NodeSeq = {
    in.flatMap(bind(_, nominee))
  }

  def bind(in : Node, nominee:Votable) : NodeSeq = {
    in match {
      
      case Elem("poll", tag, attribs, scope, children @ _*) => tag match {
      case "name" => formatNominee(nominee)
      case "id" => Text(nominee.id.toString)
      case "title" => Text(nominee.toString)
      case "result" =>
	renderVote(() => formatResult(VoteCounter.getResult(nominee).value))
      case "chart" => chart(nominee.uri, in.attributes)
      case "supporters" =>
	getSupporters().flatMap {
	  user => bind(bind(children, user, nominee), VotableUser(user))
	}
      }

      case Elem("me", tag, attribs, scope, children @ _*) =>
	currentUser match {
	  case Full(me) => tag match {
	    case "weight" =>
	      renderVote(() => {
		val format= attribs.get("format").getOrElse(Text("numer"))
		formatWeight(me, nominee, format.text)} )
	    case "up" =>
	      SHtml.a(() => vote(nominee,1), children)
	    case "down" =>
	      SHtml.a(() => vote(nominee, -1), children)
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

      case Elem("query", tag, attribs, scope, _*) =>
	nominee match {
	  case VotableQuery(query) => tag match { 
	    case "creator" => formatNominee(VotableUser(query.creator.obj.get))
	    case "time" => Text(Tick.format(query.creation.is))
	    case "keys" => 
	      buttonFactory.newKeyListRecord(() => query.keyList,
					     list => query.keys(list).save )
	      buttonFactory.toggleText
	    case "graph" => DelegationGraphView.graphNode(query)
	    case "numVoters" => 
	      renderVote(() => 
		Text(VoteCounter.getAllVoters(query).size.toString))
	    case _ => in
	  }
	  case _ => in
	}

      case Elem("user", tag, attribs, scope, children @ _*) =>
	nominee match {
	  case VotableUser(user) => tag match {
	    case "profile" => 
	      buttonFactory.newCommentRecord(() => user.profile.is, value => { user.profile(value); user.save })
	      buttonFactory.toggleText
	    case "inflow" =>  renderVote(() => formatResult(VoteCounter.getDelegationInflow(user)))
	    case "outflow" =>  renderVote(() => formatResult(VoteCounter.getDelegationOutflow(user)))
	    case "itsme" => if (Full(user)==currentUser) bind(children, nominee) else NodeSeq.Empty
	    case "notme" => if (Full(user)!=currentUser) bind(children, nominee) else NodeSeq.Empty
	    case "votes" => getVotes().flatMap { vote => bind(bind(children, user, vote), vote) }
	    case "delegates" => getDelegates().flatMap { delegate => bind(bind(children, user, VotableUser(delegate)), VotableUser(delegate)) }
	    case "emoticon" => 
	      renderVote(() => emoticon(user, attribs))
	    case "graph" => DelegationGraphView.graphNode(user)
	    case _ => in
	  }
	  case _ => tag match {
	    case "itsme" => NodeSeq.Empty
	    case "notme" => bind(children, nominee)
	    case _ => in
	  }
	}

      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, attribs, scope, bind(children, nominee) : _*)

      case other => other
    }
  }
  
  def chart(uri:String, attribs:MetaData ) : Node = {
    val options = attribs.elements.map ( a=> a.key+"="+a.value).mkString("&")
    <a href={uri+"/analyzer.html"}><embed
    alt="Chart"
    src={uri+"/chart.svg?"+options}
    width={attribs.get("width").getOrElse(Text("640"))}
    height={attribs.get("height").getOrElse(Text("480"))}/></a>
  }

  def emoticon(other : User, attribs:MetaData) : Node = {
    val size={attribs.get("size").getOrElse(Text("100"))}
    var uri= "/emoticons/face.svg" + {
      attribs.asAttrMap ++ {
	if (currentUser.isEmpty) {
	  Map("view" -> "sleeping")
	} else {
	  val emo= VoteCounter.getEmotion(currentUser.get, other)
	  if (!emo.isEmpty) {
	    Map("v" -> formatDouble(emo.get.valence.value / 2.0 + 0.5),
		"a" ->  formatDouble(emo.get.valence.swing min 1.0),
		"p" ->  formatDouble(emo.get.potency.value))
	  }
	  else
	    Map("view" -> "sleeping")
	}}
    }.map { case (a,b) => a+"="+b }.mkString("?","&","")
    <embed
    alt="Emoticon"
    src={uri} width={size} height={size}/>
  }

  def bind(in : NodeSeq, user:User, nominee:Votable) : NodeSeq = {
    in.flatMap(bind(_, user, nominee))
  }

  def bind(in : Node, user : User, nominee : Votable): NodeSeq= {

    // conditional recursive processing
    def rec(cond:Boolean) = 
      if (cond) bind(in.child, user, nominee) else NodeSeq.Empty

    in match {
      case Elem("user", label, attribs, scope, _*) => label match {
	case "name" => formatNominee(VotableUser(user))
	case "notme" => rec(currentUser!=Full(user))
	case "itsme" => rec(currentUser==Full(user))
	case "comment" => {
	  buttonFactory.newCommentRecord(
	    () => VoteCounter.getComment(user, nominee).getOrElse(""),
	    text => PollingBooth.comment(user, nominee, text))
	  buttonFactory.toggleText
	}
	case _ => Elem(prefix, label, attribs, scope, rec(true) : _*)
      }

      case Elem("poll", label, attribs, scope, _*) => label match {
	case "name" => formatNominee(nominee)
	case "notme" => rec(currentUser.isEmpty || VotableUser(currentUser.get)!=nominee)
	case "itsme" => rec(!currentUser.isEmpty && VotableUser(currentUser.get)==nominee) 
	case _ =>  Elem(prefix, label, attribs, scope, rec(true) : _*)

      }

      case Elem("vote", label, attribs, scope, _*) =>
	label match {
	  case "result"  => {
	    val format= in.attribute("format").getOrElse(Text("decimal"))
	    renderVote(() => formatWeight(user,nominee,format.text))
	  }
	  case "flow" => {
	    def denom= VoteCounter
	    .getActiveVotes(user)
	    .foldLeft(0) { 
	      (a,b) => 
		a + Math.abs(VoteCounter.getPreference(user,b)) 
	    }
	    renderVote(() => {
	      val pref= VoteCounter.getPreference(user, nominee)
	      formatResult(pref/Math.max(1.0, denom )) } ) 
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
