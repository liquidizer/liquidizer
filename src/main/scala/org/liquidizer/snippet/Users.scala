package org.liquidizer.snippet

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._

import _root_.org.liquidizer.model._
import _root_.org.liquidizer.lib._



class Users extends MultipageSnippet {

  def size= users.size

  var users= getUsers()
  def getUsers() = {
    println("loading users")
    User.findAll
    .filter { searchFilter _ }
    .sort { sortOrder() }
  }

  def sortOrder() : (User,User)=> Boolean = {
    def myvote(u:User)= userVolume(User.currentUser.get, VotableUser(u))
    def swing(u:User) = VoteCounter.getSwing(VotableUser(u))
    def quote(u:User) = VoteCounter.getResult(u)
    order match {
      case "pro" => (a,b) => quote(a).pro > quote(b).pro
      case "swing" => (a,b) => swing(a) > swing(b)
      case "age" => (a,b) => a.id.is > b.id.is
      case "myvote" => (a,b) => myvote(a) > myvote(b)
      case "alpha" => (a,b) => a.nick.is.toUpperCase < b.nick.is.toUpperCase
      case _ => (a,b) => quote(a).volume > quote(b).volume
    }
  }


  def render(in: NodeSeq) : NodeSeq = {
    val helper= new VotingHelper
    users
    .slice(from,to)
    .flatMap {
      user =>  helper.render(in, VotableUser(user))
    }
  }
}

class UserDetails extends MultipageSnippet {
  val user= User.getUser(S.param("user").openOr("-1")).get

  val votes= 
    VoteCounter.getVotes(user, true)
    .filter { searchFilter _ }
    .sort { voteSortOrder(user) }

  override def size= votes.size

  def render(in : NodeSeq) : NodeSeq = {
    val helper= new VotingHelper {
      override def getVotes(user:User) : List[Votable] = {
	votes.slice(from,to)
      }
      override def getSupporters(v:Votable) : List[User] = Nil
    }
    helper.render(in, VotableUser(user))
  }
}

class UserGraphDetails {
  val user= User.getUser(S.param("user").openOr("-1")).get

  val votes= VoteCounter.getVotes(user, false)

  def render(in : NodeSeq) : NodeSeq = {
    val helper= new VotingHelper {
      override def getVotes(user:User) : List[Votable] = 
	votes
        .filter {
	  case VotableQuery(_) => true
	  case _ => false
	}
        .sort { _.id < _.id }
    }
    helper.render(in, VotableUser(user))
  }
}

class UserSupporters extends MultipageSnippet {
  val user= User.getUser(S.param("user").openOr("-1"))

  val supporters= 
    VoteCounter.getSupporters(VotableUser(user.get), true)
    .filter { searchFilter _ }
    .sort { voteSortOrder(VotableUser(user.get)) }

  override def size= supporters.size

  val helper= new VotingHelper {
    override def getVotes(user:User) : List[Votable] = Nil
    override def getSupporters(nominee:Votable) : List[User] = {
      supporters.slice(from,to)
    }
  }

  def render(in : NodeSeq) : NodeSeq = {
    helper.render(in, VotableUser(user.get))
  }
}

class VoteDetails {
  val user= User.getUser(S.param("user").openOr("-1"))
  val helper= new VotingHelper

  val nominee= {
    if (!S.param("query").isEmpty) {
      VotableQuery(Query.getQuery(S.param("query").get).get)
    }
    else {
      VotableUser(User.getUser(S.param("user2").get).get)
    }
  }
  
  def render(in : NodeSeq) : NodeSeq = {	
    helper.bind(in, user.get, nominee)
  }
}
