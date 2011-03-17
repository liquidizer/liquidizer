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
    data = User.findAll(By(User.validated, true))
    .filter { searchFilter _ }
    .map { VotableUser(_) }
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

class UserDetails extends MultipageSnippet {

  val user= User.getUser(S.param("user").openOr("-1")).get

  /** load queries voted for by this user */
  def loadVotes() = {
    data= 
      VoteMap.getAllVotes(user)
      .filter { searchFilter _ }
      .map { VotableQuery(_) }
    sortData(user)
  }

  /** load active supporters for this user */
  def loadSupporters() = {
    data= 
      VoteMap.getActiveVoters(VotableUser(user))
      .filter { searchFilter _ }
      .map { VotableUser(_) }
    sortData(VotableUser(user))
  }

  /** load delegates chosen by this user */
  def loadDelegates() = {
    data= 
      VoteMap.getActiveVotes(user)
      .filter {
	case VotableUser(user) => searchFilter(user)
        case _ => false
      }
    sortData(user)
  }

  /** Render the HTML snippet */
  def render(in : NodeSeq) : NodeSeq = {
    val helper= new VotingHelper {
      override def getVotes() : List[Votable] = {
	if (data.isEmpty) loadVotes()
	data.slice(from,to)
      }
      override def getSupporters() : List[User] = {
	if (data.isEmpty) loadSupporters()
	data.slice(from,to).map { case VotableUser(user) => user }
      }
      override def getDelegates() : List[User] = {
	if (data.isEmpty) loadDelegates()
	data.slice(from,to).map { case VotableUser(user) => user }
      }
    }
    helper.bind(in, VotableUser(user))
  }
}

