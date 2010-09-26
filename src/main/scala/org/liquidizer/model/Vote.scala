package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

abstract class Votable {
  def id:Long
  def uri:String
}

case class VotableQuery(query : Query) extends Votable {
  def id= query.id
  def uri = "/queries/"+id
  override def toString = query.toString
}

case class VotableUser(user : User) extends Votable {
  def id= user.id
  def uri = "/users/"+id
  override def toString = user.toString
}

class Vote extends LongKeyedMapper[Vote] with IdPK {
  def getSingleton = Vote

  // who votes when
  object date extends MappedLong(this)
  object owner extends MappedLongForeignKey(this, User)
  
  // subject of vote
  object query extends MappedLongForeignKey(this, Query)
  object user extends MappedLongForeignKey(this, User)
  
  // attributes of vote
  object comment extends MappedLongForeignKey(this, Comment)
  object weight extends MappedInt(this)
  
  val format= new java.text.SimpleDateFormat("MMM-dd HH:mm:ss")
  
  def getVotable:Votable = {
	  if (query.defined_?)
		  VotableQuery(query.obj.get)
      else
    	  VotableUser(user.obj.get)
  }
  
  def setVotable(votable : Votable):Unit = votable match {
	  case VotableQuery(nominee) => query(nominee)
	  case VotableUser(nominee) => user(nominee)
  }
  
  def getOwner:User = {
      owner.obj.get
  }
  
  override def toString() : String = {
    val voteFor=
      if (query.obj.isEmpty)
    	  user.obj.get.displayName
      else
    	  query.obj.get.what.is
    format.format(new java.util.Date(date.is))+" ("+weight.is+") "+owner.obj.get.toString+" -> "+ voteFor
  }
}

object Vote extends Vote with LongKeyedMetaMapper[Vote] {
  override def dbTableName = "votes"
  override def fieldOrder = List(date, owner, query, weight)
}

