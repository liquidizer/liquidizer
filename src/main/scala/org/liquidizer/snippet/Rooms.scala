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
    if (S.param("room").isEmpty) NodeSeq.Empty else in

  /** Show this content only if the user has not entered a room */
  def out(in : NodeSeq) : NodeSeq =
    if (S.param("room").isEmpty) in else NodeSeq.Empty

  /** Show a list of all available rooms */
  def list(in : Node) : NodeSeq = {
    Room.findAll.flatMap { room =>
      Helpers.bind("room", in,
		   "name" -> <a href={"/room/"+room.id.is+"/index.html"}>{
		     room.name.is}</a>,
		   "id" -> Text(room.id.is.toString))}
  }

  /** Controls for the creation of a new room */
  def create(in : Node) : NodeSeq = {
    var name= ""
    if (User.currentUser.isEmpty) NodeSeq.Empty else {
      Helpers.bind("room", in,
		   "name" -> SHtml.text(name, name = _),
		   "submit" -> SHtml.ajaxSubmit(S ? "create", () => createRoom(name)))
    }
  }

  /** create a new room with the given name */
  def createRoom(name : String) = {
    if (name.length > 0) {
      val room= Room.create.name(name).saveMe
      PollingBooth.activate(User.currentUser.get, Some(room))
      RedirectTo("/room/"+room.id.is+"/index.html")
    } else Noop
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
