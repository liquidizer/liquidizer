package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class Vote extends LongKeyedMapper[Vote] with IdPK {
  def getSingleton = Vote

  // who votes when
  object date extends MappedLong(this)
  object owner extends MappedLongForeignKey(this, User)
  
  // subject of vote
  object nominee extends MappedLongForeignKey(this, Votable)

  // Deprecated
  object query extends MappedLongForeignKey(this, Query)
  object user extends MappedLongForeignKey(this, User)
  object comment extends MappedLongForeignKey(this, Comment)
  // Deprecated end
  
  // attributes of vote
  object weight extends MappedInt(this)
  
  val format= new java.text.SimpleDateFormat("MMM-dd HH:mm:ss")
  
  def getVotable : Votable = {

    // deprecated
    if (!nominee.defined_?) {
      val votable : Votable= 
	if (query.defined_?) VotableQuery(query.obj.get)
	else
    	  VotableUser(user.obj.get)
      nominee(votable)
      save
      votable
    } else
      // deprecated end
      nominee.obj.get
  }
  
  def getOwner:User = {
      owner.obj.get
  }
  
  override def toString() : String = {
    format.format(new java.util.Date(date.is))+
    " ("+weight.is+") "+
    getOwner.toString + " -> " + getVotable.toString
  }
}

object Vote extends Vote with LongKeyedMetaMapper[Vote] {
  override def dbTableName = "votes"
  override def fieldOrder = List(date, owner, query, weight)
}

