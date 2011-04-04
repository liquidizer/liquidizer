package org.liquidizer.snippet

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._

import _root_.org.liquidizer.model._
import _root_.org.liquidizer.lib._

class Users extends MultipageSnippet {

  def getData() = {
    if (data.isEmpty) loadData()
    data
  }
    
  def loadData() = {
    data = Votable.findAll(By_>(Votable.user, 0), By(Votable.room, room))
    .filter { _.user.obj.get.validated }
    .filter { searchFilter _ }
    sortData()
  }

  override def categories(in:NodeSeq) : NodeSeq = {
    val keys= search.split(" +").toList.map { _ toLowerCase }
    val markup= new CategoryView(keys, "/users.html")
    <span>{
      markup.renderTagList(TaggedUtils.sortedTags(data), 5)
    }</span>
  }

  def render(in: NodeSeq) : NodeSeq = {
    val helper= new VotingHelper
    getData()
    .slice(from,to)
    .flatMap {
      user =>  helper.bind(in, user)
    }
  }
}

class UserDetails extends MultipageSnippet with InRoom {

  val uid= S.param("user").get.toLong
  val user= User.find(By(User.id, uid)).get
  val nominee= toNominee(user)

  /** load queries voted for by this user */
  def loadVotes() = {
    data= 
      VoteMap.getAllVotes(user)
      .filter { searchFilter _ }
    sortData(user)
  }

  /** load active supporters for this user */
  def loadSupporters() = {
    if (!nominee.isEmpty) data= 
      VoteMap.getActiveVoters(nominee.get)
      .filter { searchFilter _ }
      .map { VotableUser(_) }
    nominee.foreach { sortData(_) }
  }

  /** load delegates chosen by this user */
  def loadDelegates() = {
    data= 
      VoteMap.getActiveVotes(user, room.get)
      .filter { n => n.isUser && searchFilter(n.user.obj.get) }
    sortData(user)
  }

  /** Render the HTML snippet */
  def render(in : NodeSeq) : NodeSeq = {
    val helper= new VotingHelper with PageHelper {
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

