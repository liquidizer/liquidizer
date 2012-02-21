package org.liquidizer.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.mapper._
import Helpers._

import org.liquidizer.model._
import org.liquidizer.lib._

/** List of rooms and room ceration */
class Rooms extends InRoom {

  /** Show this content only if the user entered a room */
  def in(in : NodeSeq) : NodeSeq =
    if (room.isEmpty) NodeSeq.Empty else in

  /** Show this content only if the user has not entered a room */
  def out(in : NodeSeq) : NodeSeq =
    if (room.isEmpty) in else NodeSeq.Empty

  /** Show a list of all available rooms */
  def list(in : Node) : NodeSeq = {
    Room.findAll.flatMap { room =>
      Helpers.bind("room", in,
		   "name" -> <a href={"/room/"+room.name.is+"/index.html"}>{
		     room.name.is}</a>,
		   "id" -> Text(room.id.is.toString))}
  }

  /** Controls for the creation of a new room */
  def create(in : Node) : NodeSeq = {
    var name= ""
    var decay= "0.01"
    var codes= ""
    var restrict= false;
    var	allowAQ= true;
    if (User.currentUser.isEmpty) NodeSeq.Empty else {
      Helpers.bind("room", in,
		   "name" -> SHtml.text(name, name = _),
		   "decay" -> SHtml.text(decay, decay = _),
		   "codes" -> <div id="inviteCodes"></div>,
		   "allowAddQuery" -> SHtml.checkbox(allowAQ, allowAQ= _),
		   "restrict" -> SHtml.ajaxCheckbox(restrict, show => {
		     restrict=show
		     if (restrict) 
		       SetHtml("inviteCodes",
			       SHtml.textarea(codes, codes = _, 
					      "id"->"inviteCodes",
					      "rows"->"15", "cols"->"30"))
		     else 
		       SetHtml("inviteCodes", NodeSeq.Empty)
		   }),
		   "submit" ->
		   SHtml.ajaxSubmit(S ? "create", () => {
		     val codeOpt= if (restrict) Some(codes) else None
		     createRoom(name.trim, decay, allowAQ, codeOpt)
		   })
		 )
    }
  }

  /** create a new room with the given name */
  def createRoom(name : String, decay : String, 
		 allowAddQueries : Boolean,
		 codes : Option[String]) = {
    if (name.length == 0) {
      Noop
    } 
    else {
      if (Room.get(name).isEmpty) {
	val room= Room.create.name(name)
	room.owner(User.currentUser.get)
	room.decay(decay.toDouble)
	room.needsCode(!codes.isEmpty)
	room.fixedQueries(!allowAddQueries)
	room.save
	if (room.needsCode.is) {
	  val list= codes.get.split("\n").map { _.trim }.filter{ _.length > 0 }
	  for (code <- list) {
	    val codeItem= InviteCode.create
	    codeItem.room(room)
	    codeItem.code(code)
	    codeItem.save
	  }
	} else {
	  PollingBooth.activate(User.currentUser.get, Some(room), "")
	}
      }
      RedirectTo("/room/"+name+"/index.html")
    } 
  } 

  /** List of new users to be shown in the sidebar */
  def newUsers(in : NodeSeq) : NodeSeq = {
    val data= Votable.findAll(By(Votable.room, room.get),
			      By_>(Votable.user, 0),
			      OrderBy(Votable.id, Descending), MaxRows(5))
    val helper= new VotingHelper
    data.flatMap { helper.bind(in, _) }
  }

  /** List of new queries to be shown in the sidebar */
  def newQueries(in : NodeSeq) : NodeSeq = {
    val data= Votable.findAll(By(Votable.room, room.get),
			      By_>(Votable.query, 0),
			      OrderBy(Votable.id, Descending), MaxRows(4))
    val helper= new VotingHelper
    data.flatMap { helper.bind(in, _) }
  }

}
