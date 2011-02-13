package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

object VotableQuery {
  def apply(query : Query) : Votable = query.nominee.obj.get
  def unapply(nominee : Votable) : Option[Query] =
    if (nominee.isQuery) Some(nominee.query.obj.get) else None
}
object VotableUser {
  def apply(user : User) : Votable = user.nominee.obj.get
  def unapply(nominee : Votable) : Option[User] =
    if (nominee.isUser) Some(nominee.user.obj.get) else None
}

class Votable extends LongKeyedMapper[Votable] with IdPK {
  def getSingleton = Votable

  // subject of vote
  object query extends MappedLongForeignKey(this, Query)
  object user extends MappedLongForeignKey(this, User)
  
  def isQuery() = query.defined_?
  def isUser() = user.defined_?
  def baseId() = if (isQuery) query.obj.get.id.is else user.obj.get.id.is

  def uri : String = this match {
    case VotableUser(user) => "/users/"+user.id.is
    case VotableQuery(query) => "/queries/"+query.id.is
  }

  override def toString() = this match {
    case VotableUser(user) => user.toString
    case VotableQuery(query) => query.toString
    case _ => "-" // might be called before fully defined
  }
}

object Votable extends Votable with LongKeyedMetaMapper[Votable] {
  override def dbTableName = "nominees"
  override def fieldOrder = List(user, query)
}

