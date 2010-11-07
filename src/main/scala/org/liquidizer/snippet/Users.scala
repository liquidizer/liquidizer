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
    data = User.findAll
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
      user =>  helper.render(in, user)
    }
  }
}

class UserDetails extends MultipageSnippet {

  val user= User.getUser(S.param("user").openOr("-1")).get

  def loadVotes() = {
    data= 
      VoteCounter.getAllVotes(user)
      .filter { searchFilter _ }
      .map { VotableQuery(_) }
    sortData(user)
  }
  def loadSupporters() = {
    data= 
      VoteCounter.getActiveVoters(VotableUser(user))
      .filter { searchFilter _ }
      .map { VotableUser(_) }
  }
  def loadDelegates() = {
    data= 
      VoteCounter.getActiveVotes(user)
      .filter { 
	case VotableUser(user) => searchFilter(user)
        case _ => false
      }
  }

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
    helper.render(in, VotableUser(user))
  }
}

