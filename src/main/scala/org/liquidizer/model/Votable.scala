package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

object VotableQuery {
  def unapply(nominee : Votable) : Option[Query] = nominee.query.obj
}

object VotableUser {
  def unapply(nominee : Votable) : Option[User] = nominee.user.obj
}

/** Represents a user or a query as a presence in a certain room */
class Votable extends LongKeyedMapper[Votable] with IdPK {
  def getSingleton = Votable

  // subject of vote
  object query extends MappedLongForeignKey(this, Query) with DBIndexed
  object user extends MappedLongForeignKey(this, User) with DBIndexed
  
  // room in which this vote is placed
  object room extends MappedLongForeignKey(this, Room) with DBIndexed

  def isQuery() = query.defined_?
  def isUser() = user.defined_?
  def is(other : User) = user.is == other.id.is

  override def toString() = this match {
    case VotableUser(user) => user.toString
    case VotableQuery(query) => query.toString
    case _ => "-" // might be called before fully defined
  }
}

/** Represents a user or a query as a presence in a certain room */
object Votable extends Votable with LongKeyedMetaMapper[Votable] {
  override def dbTableName = "nominees"
  override def fieldOrder = List(user, query, room)

  def get(id : Long) = find(By(this.id, id))
  def getQuery(id : Long, room : Room) = find(By(query,id), By(this.room,room))
  def getUser(id : Long, room : Room) = find(By(user,id), By(this.room,room))

  def get(users : List[User], room : Room) =
    findAll(ByList(user, users.map { _.id.is }), By(this.room, room))
}

