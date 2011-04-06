package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class Comment extends LongKeyedMapper[Comment] with IdPK {
  def getSingleton = Comment

  object date extends MappedLong(this)
  object author extends MappedLongForeignKey(this, User)
  object nominee extends MappedLongForeignKey(this, Votable)
  object content extends MappedText(this)
  
  val format= new java.text.SimpleDateFormat("HH:mm:ss")
  override def toString() : String = {
    format.format(new java.util.Date(date.is))+content.is
  }
}

object Comment extends Comment with LongKeyedMetaMapper[Comment] {
  override def dbTableName = "comments"
  override def fieldOrder = List(date, author, nominee, content)

  def get(id : Long) : Option[Comment] = find(By(Comment.id, id))

  def get(author : User, nominee : Votable) : Option[Comment] =
    find(By(Comment.author, author), By(Comment.nominee, nominee))

  def getText(author : User, nominee : Votable) : String =
    get(author, nominee). map { _.content.is }.getOrElse("")

  def getTime(author : User, nominee : Votable) : Long =
    get(author, nominee). map { _.date.is }.getOrElse(0L)

  def getLatest(nominee : Votable) : Option[Comment] =
    find(By(Comment.nominee, nominee), 
	 OrderBy(Comment.date, Descending), MaxRows(1))

  def getLatest(user : User) : Option[Comment] =
    find(By(Comment.author, user), 
	 OrderBy(Comment.date, Descending), MaxRows(1))


}

