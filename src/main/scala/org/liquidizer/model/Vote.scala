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

  // attributes of vote
  object weight extends MappedInt(this)
  
  val format= new java.text.SimpleDateFormat("MMM-dd HH:mm:ss")
  
  def getVotable : Votable = nominee.obj.get
  def getOwner:User = owner.obj.get
  
  override def toString() : String = {
    format.format(new java.util.Date(date.is))+
    " ("+weight.is+") "+
    getOwner.toString + " -> " + getVotable.toString
  }
}

object Vote extends Vote with LongKeyedMetaMapper[Vote] {
  override def dbTableName = "votes"
  override def fieldOrder = List(date, owner, nominee, weight)

  def get(owner : User, nominee : Votable) : Option[Vote] =
    find(By(Vote.owner, owner), By(Vote.nominee, nominee))

  def get(owner : User) : List[Vote] = findAll(By(Vote.owner, owner))
  def get(nominee : Votable) : List[Vote] = findAll(By(Vote.nominee, nominee))
}

