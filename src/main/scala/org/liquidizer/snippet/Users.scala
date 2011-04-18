package org.liquidizer.snippet

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._

import _root_.org.liquidizer.model._
import _root_.org.liquidizer.lib._

/** Snippet code to generate a list of all users */
class Users extends MultipageSnippet {

  /** load user data */
  def getData() = {
    if (data.isEmpty && !room.isEmpty) {
      data = Votable.findAll(By_>(Votable.user, 0), By(Votable.room, room.get))
      .filter { _.user.obj.get.validated }
      data= data.filter { searchFilter _ }
      sortData()
    }
    data
  }
    
  /** List of all hash tags in user comments */
  override def categories(in:NodeSeq) : NodeSeq = {
    val keys= search.split(" +").toList.map { _ toLowerCase }
    val markup= new CategoryView(keys, "/users.html")
    <span>{ markup.renderTagList(tags.sortedTags(data), 10) }</span>
  }

  /** Render list of all users in the current room */
  def render(in: NodeSeq) : NodeSeq = {
    val helper= new VotingHelper
    getData()
    .slice(from,to)
    .flatMap {
      user =>  helper.bind(in, user)
    }
  }
}

/** Snippet code to display details on a user's voting behaviour */
class UserDetails extends MultipageSnippet with InRoom {

  val uid= S.param("user").get.toLong
  val user= User.find(By(User.id, uid)).get
  val nominee= toNominee(user)

  /** load queries voted for by this user */
  def loadVotes() = {
    if (!room.isEmpty) 
      data= 
	VoteMap.getAllVotes(user, room.get).filter { searchFilter _ }
    sortData(user)
  }

  /** load active supporters for this user */
  def loadSupporters() = {
    if (!nominee.isEmpty) {
      data= Votable.get(
	VoteMap.getActiveVoters(nominee.get)
        .filter { u => searchFilter(u.toString) }, room.get)
      sortData(nominee.get) 
    }
  }

  /** load delegates chosen by this user */
  def loadDelegates() = {
    data= 
      VoteMap.getActiveVotes(user, room.get)
      .filter { n => n.isUser && searchFilter(n.toString) }
    sortData(user)
  }

  /** Render the HTML snippet */
  def render(in : NodeSeq) : NodeSeq = {
    val helper= new VotingHelper with MultiPageHelper {
      override def getData(src : String) : List[Votable] = {
	if (data.isEmpty) src match {
	  case "votes" => loadVotes
	  case "supporters" => loadSupporters
	  case "delegates" => loadDelegates
	}
	data
      }
    }
    if (nominee.isEmpty) NodeSeq.Empty
    else helper.bind(in, nominee.get)
  }
}

/** List of votes by the current user, to be shown in the overview page */
class UserOverview extends InRoom{
  def render(in : NodeSeq) : NodeSeq = {
    User.currentUser match {
      case Full(me) if !room.isEmpty => 
	val nominee= toNominee(me)
	val helper= new VotingHelper {
	  val listVotes= VoteMap.getActiveVotes(me, room.get)
	    .sort { _.id.is > _.id.is }
	  val listQueryVotes= listVotes.filter { _.isQuery }
	  val listSupporters= toNominee(me).map {
	    VoteMap.getActiveVoters(_).sort { _.id.is > _.id.is } }
            .getOrElse(Nil)
          val listUserVotes= listVotes
	    .filter { _.isUser }

	  override def slice(list : List[Votable]) = list.slice(0, 10)
	  override def getData(src : String) = src match {
	    case "votes" => listQueryVotes
	    case "supporters" => listSupporters.map { toNominee(_).get }
	    case "delegates" => listUserVotes
	  }
	}
        if (nominee.isEmpty) NodeSeq.Empty
        else helper.bind(in, nominee.get)
      case _ => NodeSeq.Empty
    }
  }
}
