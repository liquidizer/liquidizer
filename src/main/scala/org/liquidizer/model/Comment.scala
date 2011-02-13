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
  
  def getAuthor() : User = author.obj.get
  def getVotable() : Votable = nominee.obj.get

  val format= new java.text.SimpleDateFormat("HH:mm:ss")
  override def toString() : String = {
    format.format(new java.util.Date(date.is))+content.is
  }
}

object Comment extends Comment with LongKeyedMetaMapper[Comment] {
  override def dbTableName = "comments"
  override def fieldOrder = List(date, author, nominee, content)

  def getComment(id : Long) : Option[Comment] = find(By(Comment.id, id))
}

