package org.liquidizer.snippet

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util.Mailer._  
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

  /** Standard profile settings of the user */
  def bind(in : Node) : NodeSeq = {
    User.currentUser match {
      case Full(user) =>
      in match { 
	case Elem("me", label, attribs, scope, children @ _*) => label match {
	  case "name" => <span>{user.displayName}</span>
	  case "logout" => SHtml.a(() => {
	    User.logUserOut(); RedirectTo("/") }, children)
	  case "passwd" =>
	    <a href={"/"+User.changePasswordPath.mkString("/")}>{children}</a>

	  case "profile" =>
	    buttonFactory.newCommentRecord(() => user.profile.is, value => { user.profile(value); user.save })
	  buttonFactory.toggleText
	  case "profileEdit" =>
	    buttonFactory.toggleButton
	  
	  case "email" =>
	    buttonFactory.newLineRecord(() => user.email.is, value => { user.email(value); user.save })
	  buttonFactory.toggleText
	  case "emailEdit" =>
	    buttonFactory.toggleButton
	  case _ => Elem("me", label, attribs, scope, bind(children) : _*)

	}
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

  /** Loggin a user with name and password */
  def login(username:String, password:String):Unit = {
    User.getUserByNick(username) match {
      case Some(user) if user.validated && user.password.match_?(password) =>  
	User.logUserIn(user)
        S.redirectTo("/")
      case Some(user) if !user.validated =>  
	S.error(S.??("account.validation.error"))  
      case Some(user) =>
	S.error("Ungültiges Passwort")
      case _ => 
	S.error("User existiert nicht")
    }  
  }  
  
  /** Show this content only if the user is logged out */
  def in(in:NodeSeq) : NodeSeq = {
    if (User.currentUser.isEmpty)
      NodeSeq.Empty
    else
      bind(in)
  }

  /** Show this content only if the user is logged in */
  def out(in:NodeSeq) : NodeSeq = {
    if (User.currentUser.isEmpty)
      bind(in)
    else
      NodeSeq.Empty
  }

  /** List of recent votes by the current user, to be shown in the index page */
  def votes(in : NodeSeq) : NodeSeq = {
    val length= 10
    User.currentUser match {
      case Full(me) => 
	val helper= new VotingHelper {
	  override def getVotes() : List[Votable] =
	    VoteCounter.getActiveVotes(me)
	    .filter {
	      case d : VotableQuery => true
	      case _ => false
	    }.slice(0,length)

	  override def getSupporters() : List[User] =
	    VoteCounter.getActiveVoters(VotableUser(me)).slice(0,length)

	  override def getDelegates() : List[User] =
	    VoteCounter.getActiveVotes(me)
	    .flatMap {
	      case VotableUser(user) => List(user)
	      case _ => Nil
	    }.slice(0,length)
	}
      helper.render(in, VotableUser(me))
      case _ => NodeSeq.Empty
    }
  }

  /** List of new users to be shown in the sidebar */
  def newUsers(in : NodeSeq) : NodeSeq = {
    val helper= new VotingHelper
    User
    .findAll(OrderBy(User.id,Descending)).slice(0,5)
    .flatMap { user => helper.render(in, VotableUser(user)) }
  }

  /** List of new queries to be shown in the sidebar */
  def newQueries(in : NodeSeq) : NodeSeq = {
    val helper= new VotingHelper
    Query
    .findAll(OrderBy(Query.id,Descending)).slice(0,4)
    .flatMap { query => helper.render(in, VotableQuery(query)) }
  }

  /** change the password */
  def passwd(in : NodeSeq) : NodeSeq = {
     val user = User.currentUser.open_!
     var passwd1= ""
     var passwd2= ""
   
     def setPasswd() {  
	if (passwd1!=passwd2) {
	  S.error("Passwörter stimmen nicht überein")
	} 
	else if (passwd1.length<=4) {
	  S.error("Passwort zu kurz")
	}
        else {
	  user.password(passwd1)  
	  user.save
	  S.notice(S.??("password.changed"))
	  S.redirectTo("/")  
       }  
     }  
   
     Helpers.bind("user", in,
          "new_pwd1" -> SHtml.password("", passwd1 = _ ),
          "new_pwd2" -> SHtml.password("", passwd2 = _ ),
          "submit" -> SHtml.submit(S.??("change"), setPasswd _))
  }

  /** resend a lost password */
  def resend(in : NodeSeq) = {
     var email= ""
   
     def sendPasswd() {
       if (!email.contains("@")) {
	 S.error("Email adresse ist ungültig")
       }
       else {
         User.find(By(User.email, email)) match {
	   case Full(user) => {
             user.uniqueId.reset().save  
             val resetLink = S.hostAndPath+User.passwordResetPath.mkString("/", "/", "/")+user.uniqueId

             val msgXml =   (<html>  
			     <head>  
			     <title>{S.??("reset.password.confirmation")}</title>  
			     </head>  
			     <body>  
			     <p>{S.??("dear")} {user.nick},  
			     <br/>  
			     {S.??("click.reset.link")}  
			     <br/><a href={resetLink}>{resetLink}</a>  
			     <br/>  
			     {S.??("thank.you")}  
			     </p>  
			     </body>  
			     </html>)

             Mailer.sendMail(From(User.emailFrom), Subject(User.passwordResetEmailSubject),  
                           To(user.email), xmlToMailBodyType(msgXml))
             S.notice(S.??("password.reset.email.sent"))  
             S.redirectTo("/")
	   }
	   case _ =>
	     S.error("Kein User mit dieser Adresse gefunden")
	 }
       }  
     }  
   
     Helpers.bind("user", in,
          "email" -> SHtml.text("", email = _ ),
          "submit" -> SHtml.submit(S.??("send"), sendPasswd _))
  }
}

/** Signup snippet. */
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
	S.notice("User '"+username+"' is bereits vergeben")
      case None => 
	if (username.isEmpty) {
	  S.error("Username darf nicht leer sein")
	}
	else if (passwd1!=passwd2) {
	  S.error("Passwörter stimmen nicht überein")
	} 
	else if (passwd1.length<=4) {
	  S.error("Passwort zu kurz")
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

object UserInfo {
   def passwordReset(id: String) =  
     User.find(By(User.uniqueId, id)) match {  
       case Full(user) =>  
         User.logUserIn(user)
         S.redirectTo("/user_mgt/change_password")
       case _ =>
	 S.error(S.??("pasword.link.invalid"))
         S.redirectTo("/")
   }  
}

