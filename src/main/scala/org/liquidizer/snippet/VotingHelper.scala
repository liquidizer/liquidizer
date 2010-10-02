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

class VotingHelper {
  val currentUser= User.currentUser

  val buttonFactory = new EditButtonToggler
  
  var displayedVotes = List[()=>Node]()
  var displayedEmos = List[()=>Node]()
  var containsMyVote = false
  
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

  def formatResult(nominee : Votable):Node = {
    formatResult(VoteCounter.getResult(nominee).value)
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
    <a href={nominee.uri }>{nominee.toString}</a>
  
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
    displayedEmos.map {
      emoticon => 
	index2-= 1
	SetHtml("dynamicemo"+index2, emoticon())
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
      case <poll:name/> => formatNominee(nominee)
      case <poll:result/> =>
	renderVote(() => formatResult(nominee))

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
	    case _ => in
	  }
	  case _ => in
	}

      case Elem("user", tag, attribs, scope, children @ _*) =>
	nominee match {
	  case VotableUser(user) => tag match {
	    case "itsme" => if (Full(user)==currentUser) bind(children, nominee) else NodeSeq.Empty
	    case "notme" => if (Full(user)!=currentUser) bind(children, nominee) else NodeSeq.Empty
	    case "profile" => Markup.renderComment(user.profile.is)
	    case "votes" => getVotes(user).flatMap {
	      vote => bind(bind(children, user, vote), vote)
	    }
	    case "emoticon" => 
	      renderVote(() => emoticon(user, attribs))

	    case _ => in
	  }
	  case _ => tag match {
	    case "itsme" => NodeSeq.Empty
	    case "notme" => bind(children, nominee)
	    case _ => in
	  }
	}
      case Elem("poll", "chart", attribs, scope, children @ _*) => {
	chart(nominee.uri, attribs)
      }
      case Elem("a", tag, attribs, scope, children @ _*) => 
	val uri=tag match {
	  case "graph" => nominee.uri+"/delegation"
	  case "hist" => nominee.uri+"/histogram"
	  case "voteIn" => nominee.uri+"/support"
	  case "voteOut" => nominee.uri
	}
        if (uri!=S.uri)
	  <a href={uri}>{children}</a>
        else
	  {children}

      case Elem("poll", "supporters", attribs, scope, children @ _*) =>
	getSupporters(nominee).flatMap {
	  user => bind(bind(children, user, nominee), VotableUser(user))
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
		"a" ->  formatDouble(emo.get.valence.swing),
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
    in match {
      case <user:name/> => formatNominee(VotableUser(user))
      case <poll:id/> => Text(nominee.id.toString)
      case <poll:name/> => {
	<a href={nominee.uri}>{
	  nominee match {
	    case VotableUser(_) => "\u2192 "+ nominee.toString
	    case _ => nominee.toString
	  }}</a>
      } 
      
      case <user:notme>{children @ _*}</user:notme> =>
	if (currentUser!=Full(user)) bind(children, user, nominee) else NodeSeq.Empty
      case <user:itsme>{children @ _*}</user:itsme> =>
	if (currentUser==Full(user)) bind(children, user, nominee) else NodeSeq.Empty
      
      case <poll:notme>{children @ _*}</poll:notme> => 
	if (currentUser.isEmpty ||
 	    VotableUser(currentUser.get)!=nominee) bind(children, user, nominee) else NodeSeq.Empty
      case <poll:itsme>{children @ _*}</poll:itsme> => 
	if (!currentUser.isEmpty &&
 	    VotableUser(currentUser.get)==nominee) bind(children, user, nominee) else NodeSeq.Empty

      case <user:comment/> => {
	buttonFactory.newCommentRecord(
	    () => VoteCounter.getComment(user, nominee).getOrElse(""),
	    text => PollingBooth.comment(user, nominee, text))
	buttonFactory.toggleText
      }

      case <vote:result/> => {
	containsMyVote |= Full(user)==currentUser
	val format= in.attribute("format").getOrElse(Text("decimal"))
	renderVote(() => formatWeight(user,nominee,format.text))
      }
      case Elem("vote", "chart", attribs, scope, children @ _*) => {
	<span>Not supported</span>
      }
      case Elem(prefix, label, attribs, scope, children @ _*) =>
	Elem(prefix, label, attribs, scope, bind(children, user, nominee) : _*)
      case other => other
    }
  }

  def getVotes(user:User) : List[Votable] = {
    VoteCounter.getVotes(user, true)
  }

  def getSupporters(nominee:Votable) : List[User] = {
    VoteCounter.getSupporters(nominee, true)
  }
}
