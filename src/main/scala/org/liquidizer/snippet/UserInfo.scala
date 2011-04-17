package org.liquidizer.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.Mailer._  
import Helpers._

import org.liquidizer.model._
import org.liquidizer.lib._
import org.liquidizer.lib.ssl._

class UserInfo {
  val buttonFactory = new EditButtonToggler()

  var username=""
  var passwd=""

  def bind(in : NodeSeq) : NodeSeq = {
    in.flatMap(bind(_))
  }

  /** Standard profile settings of the user */
  def bind(in : Node) : NodeSeq = {
    // check if user is logged in
    User.currentUser match {
      case Full(me) =>
      in match { 
	case Elem("me", label, attribs, scope, children @ _*) => label match {
	  // session info
	  case "name" => <span>{me.nick.is}</span>
	  case "logout" => SHtml.a(() => {
	    User.logUserOut()
	    RedirectTo("/index.html") }, children)

	  // profile management
	  case "profile" =>
	    buttonFactory.newCommentRecord(() => me.profile.is, value => { me.profile(value); me.save })
	  buttonFactory.toggleText
	  case "email" =>
	    buttonFactory.newLineRecord(() => me.email.is, value => { me.email(value); me.save })
	  buttonFactory.toggleText
	  case "editButton" => buttonFactory.toggleButton

	  // user statistics
	  case "numVotes" => 
	    Text(VoteMap.getActiveVotes(me).filter { _.isQuery }.size.toString)
	  case "numDelegates" =>
	    Text(VoteMap.getActiveVotes(me).filter { _.isUser }.size.toString)
	  case "numSupporters" =>
	    Text(VoteMap.getActiveVoters(VotableUser(me)).size.toString)

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
	  case <me:submit/> => SHtml.ajaxSubmit(S?"system.login", () => {
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
        // restore voting weight to 1.00
        PollingBooth.vote(user, VotableUser(user), 0)
        S.redirectTo("/index.html")
      case Some(user) if !user.validated =>  
	S.error(S.??("account.validation.error"))  
      case Some(user) =>
	S.error(S ? "error.invalid.password")
      case _ => 
	S.error(S ? "error.user.not.exists")
    }  
  }  
  
  /** Show this content only if the user is logged out */
  def in(in:NodeSeq) : NodeSeq =
    if (User.currentUser.isEmpty) NodeSeq.Empty else bind(in)

  /** Assert that the user is logged in, else show error */
  def assertIn(in : NodeSeq) : NodeSeq =
    if (User.currentUser.isEmpty) 
      Text(S ? "error.not.logged.in") else bind(in)

  /** Show this content only if the user is logged in */
  def out(in:NodeSeq) : NodeSeq = 
    if (User.currentUser.isEmpty) bind(in) else NodeSeq.Empty

  /** List of recent votes by the current user, to be shown in the index page */
  def votes(in : NodeSeq) : NodeSeq = {
    val length= 10
    User.currentUser match {
      case Full(me) => 
	val helper= new VotingHelper {
	  override def getVotes() : List[Votable] =
	    VoteMap.getActiveVotes(me)
	    .filter { _.isQuery }
	    .sort { _.id.is > _.id.is }
	    .slice(0,length)

	  override def getSupporters() : List[User] =
	    VoteMap.getActiveVoters(VotableUser(me))
	    .sort { _.id.is > _.id.is }
	    .slice(0,length)

	  override def getDelegates() : List[User] =
	    VoteMap.getActiveVotes(me)
	    .filter { _.isUser }
	    .sort { _.id.is > _.id.is }
	    .map { _.user.obj.get }
	    .slice(0,length)
	}
      helper.bind(in, VotableUser(me))
      case _ => NodeSeq.Empty
    }
  }

  /** List of new users to be shown in the sidebar */
  def newUsers(in : NodeSeq) : NodeSeq = {
    val helper= new VotingHelper
    User
    .findAll(By(User.validated, true), OrderBy(User.id,Descending)).slice(0,5)
    .flatMap { user => helper.bind(in, VotableUser(user)) }
  }

  /** List of new queries to be shown in the sidebar */
  def newQueries(in : NodeSeq) : NodeSeq = {
    val helper= new VotingHelper
    Query
    .findAll(OrderBy(Query.id, Descending)).slice(0,4)
    .flatMap { query => helper.bind(in, VotableQuery(query)) }
  }

  /** change the password */
  def passwd(in : NodeSeq) : NodeSeq = {
     val user = User.currentUser.open_!
     var passwd1= ""
     var passwd2= ""
   
     def setPasswd() {  
	if (passwd1!=passwd2) {
	  S.error(S ? "error.password.not.match")
	} 
	else if (passwd1.length<=4) {
	  S.error(S ? "error.password.too.short")
	}
        else {
	  user.password(passwd1)  
	  user.save
	  S.notice(S.??("password.changed"))
	  S.redirectTo("/index.html")  
       }  
     }  
   
     Helpers.bind("user", assertIn(in),
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
	     S.error(S ? "error.no.user.for.email")
	 }
       }  
     }  
   
     Helpers.bind("user", in,
          "email" -> SHtml.text("", email = _ ),
          "submit" -> SHtml.submit(S.??("send"), sendPasswd _))
  }
}

/** Data deletion snippet */
class UserReset extends StatefulSnippet {
  var deleteVotes= false
  var deleteComments= false
  var deleteAccount= false
  
  var dispatch : DispatchIt = {
    case "render" => render _
  }

  /** render the data deletion request form */
  def render(in: NodeSeq): NodeSeq = {
    var passwd1= ""
    var passwd2= ""
    Helpers.bind("user", in, 
		 "deleteVotes" -> SHtml.checkbox(false, deleteVotes = _),
		 "deleteComments" -> SHtml.checkbox(false, deleteComments = _),
		 "deleteAccount" -> SHtml.checkbox(false, deleteAccount = _),
		 "submit" -> SHtml.submit(S ? "data.delete.confirm", () => process()))
  }

  /** process the deletion request */
  def process() = {
    val user= User.currentUser.get
    if (deleteVotes || deleteAccount) {
      PollingBooth.clearVotes(user)
      S.notice(S ? "data.delete.votes.succ")
    }
    if (deleteComments || deleteAccount) {
      PollingBooth.clearComments(user)
      S.notice(S ? "data.delete.comment.succ")
    }
    if (deleteAccount) {
      User.logUserOut()
      Vote.findAll(By(Vote.nominee, VotableUser(user))).foreach { _.delete_! }
      user.profile("").email("").nick("---").validated(false)
      user.save
      S.notice(S ? "data.delete.account.succ")
    }
    this.unregisterThisSnippet
    S.redirectTo("/index.html")
  }
}

/** Signup snippet. */
class UserSignUp extends StatefulSnippet {
  val code= InviteCode.get(S.param("code").getOrElse(""))
  var username= SSLClient.valid_certificates map { certs =>
      certs.headOption map { head =>
          PrincipalUtils.name_as_map(head.getSubjectX500Principal)("CN")
      } getOrElse ""
      //PrincipalUtils.name_as_map(certs(0).getSubjectX500Principal)("CN")
  } openOr ""
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
		 "submit" -> SHtml.submit(S?"system.signup", () => {
		   signup(passwd1, passwd2)
		 })
	       )
  }

  def signup(passwd1: String, passwd2: String) = {
    username= username.trim
    User.getUserByNick(username) match {
      case Some(user) =>
	S.notice((S ? "error.user.exists").format(username))
        redirectTo(S.uri+"?code="+code.map { _.code.is }.getOrElse(""))
      case None => 
	if (username.isEmpty) {
	  S.error(S ? "error.name.is.empty")
	}
	else if (passwd1!=passwd2) {
	  S.error(S ? "error.password.not.match")
	} 
        else if (code.isEmpty || code.get.user.exists { _.validated } ) {
	  S.error(S ? "Invite-Code invalid") // Should not get here
	} else {
	  val user= User.create
	  .nick(username)
	  .email(email)
	  .validated(User.skipEmailValidation)
	  if (passwd1.length > 4) {
	    user.password(passwd1)
	  }
	  user.save
	  code.get.user(user).save

	  User.logUserIn(user)
	  this.unregisterThisSnippet
	  redirectTo("/index.html")
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
         S.redirectTo("/index.html")
   }  
}

