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

class UserInfo {
  val buttonFactory = new EditButtonToggler()

  var username=""
  var passwd=""

  def bind(in : NodeSeq) : NodeSeq = {
    in.flatMap(bind(_))
  }

  def bind(in : Node) : NodeSeq = {
    User.currentUser match {
      case Full(user) =>
	val email = user.email
      in match { 
	case <me:name/> => <span>{user.displayName}</span>
	case <me:logout>{ch @ _*}</me:logout> => 
	  <a href={"/"+User.logoutPath.mkString("/")}>{ch}</a>
	
	case <me:passwd>{ch @ _*}</me:passwd> => 
	  <a href={"/"+User.changePasswordPath.mkString("/")}>{ch}</a>
	
	case <me:realname/> => 
	  buttonFactory.newLineRecord(() => user.realname.is, value => { user.realname(value); user.save })
	  buttonFactory.toggleText
	case <me:realnameEdit/> => 
	  buttonFactory.toggleButton
	
	case <me:homepage/> => 
	  buttonFactory.newLineRecord(() => user.homepage.is, value => { user.homepage(value); user.save })
	  buttonFactory.toggleText
	case <me:homepageEdit/> => 
	  buttonFactory.toggleButton
	
	case <me:email/> => 
	  buttonFactory.newLineRecord(() => user.email.is, value => { user.email(value); user.save })
	  buttonFactory.toggleText
	case <me:emailEdit/> => 
	  buttonFactory.toggleButton

	case Elem(prefix, label, attribs, scope, children @ _*) =>
	  Elem(prefix, label, attribs, scope, bind(children) : _*)
	case _ => in
      }
      case _ =>
	in match {
	  case <me:name/> => SHtml.text(username, username = _)
	  case <me:password/> => SHtml.password(passwd, passwd = _)
	  case <me:submit/> => SHtml.ajaxSubmit("login", () => {
	    login(username, passwd)
	    S.redirectTo(S.uri) 
	  })
	  case Elem(prefix, label, attribs, scope, children @ _*) =>
	  Elem(prefix, label, attribs, scope, bind(children) : _*)
	  case _ => in
	}
    }
  }

  def login(username:String, password:String):Unit = {
    User.getUserByNick(username) match {
      case Some(user) if user.validated && user.password.match_?(password) =>  
      User.logUserIn(user); S.notice(S.??("logged.in"));S.redirectTo("/")
      case Some(user) if !user.validated =>  
      S.error(S.??("account.validation.error"))  
      case _ => S.error(S.??("invalid.credentials"))  
    }  
  }  
  
  def in(in:NodeSeq) : NodeSeq = {
    if (User.currentUser.isEmpty)
      NodeSeq.Empty
    else
      bind(in)
  }

  def out(in:NodeSeq) : NodeSeq = {
    if (User.currentUser.isEmpty)
      bind(in)
    else
      NodeSeq.Empty
  }

  def votes(in : NodeSeq) : NodeSeq = {
    User.currentUser match {
      case Full(me) => 
	val helper= new VotingHelper {
	  override def getVotes(user:User) : List[Votable] =
	    VoteCounter.getVotes(me, false).slice(0,5)

	  override def getSupporters(nominee:Votable) : List[User] =
	    VoteCounter.getSupporters(VotableUser(me), false).slice(0,4)
	}
      helper.render(in, VotableUser(me))
      case _ => NodeSeq.Empty
    }
  }

  def newUsers(in : NodeSeq) : NodeSeq = {
    val helper= new VotingHelper
    User
    .findAll(OrderBy(User.id,Descending)).slice(0,5)
    .flatMap { user => helper.render(in, VotableUser(user)) }
  }

  def newQueries(in : NodeSeq) : NodeSeq = {
    val helper= new VotingHelper
    Query
    .findAll(OrderBy(Query.id,Descending)).slice(0,4)
    .flatMap { query => helper.render(in, VotableQuery(query)) }
  }
}

class UserSignUp extends StatefulSnippet {
  var username= ""
  var email= ""
  
  var dispatch : DispatchIt = {
    case "render" => render _
  }
  
  def render(in: NodeSeq): NodeSeq = {
    var passwd1= ""
    var passwd2= ""
    Helpers.bind("user", in, 
		 "name" -> SHtml.text(username, username = _),
		 "email" -> SHtml.text(email, email = _),
		 "passwd1" -> SHtml.password(passwd1, passwd1 = _),
		 "passwd2" -> SHtml.password(passwd2, passwd2 = _),
		 "submit" -> SHtml.submit("signup", () => {
		   signup(passwd1, passwd2)
		 })
	       )
  }

  def signup(passwd1: String, passwd2: String) = {
    username= username.trim
    User.getUserByNick(username) match {
      case Some(user) =>
      S.notice("User '"+username+"' in use")
      case None => 
      if (passwd1.length<=4) {
	S.error("Password too short")
      }
      else if (passwd1!=passwd2) {
	S.error("Passwords don't match")
      } 
      else if (username.isEmpty) {
	S.error("Username must not be empty")
      }
      else {
	val user= User.create
	.nick(username)
	.email(email)
	.password(passwd1)
	.validated(User.skipEmailValidation)
	user.save
	User.logUserIn(user)
	this.unregisterThisSnippet
	redirectTo("/")
      }
    }
  }
}

