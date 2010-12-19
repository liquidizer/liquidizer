package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class Comment extends LongKeyedMapper[Comment] with IdPK {
  def getSingleton = Comment

  object date extends MappedLong(this)
  object vote extends MappedLongForeignKey(this, Vote)
  object content extends MappedText(this)
  
  def getAuthor() : User = {
    vote.obj.get.owner.obj.get
  }
  
  def getVotable() : Votable = {
    vote.obj.get.getVotable
  }

  val format= new java.text.SimpleDateFormat("HH:mm:ss")
  override def toString() : String = {
    format.format(new java.util.Date(date.is))+content.is
  }
}

object Comment extends Comment with LongKeyedMetaMapper[Comment] {
  override def dbTableName = "comments"
  override def fieldOrder = List(date, vote, content)

  def getComment(id : Long) : Option[Comment] = find(By(Comment.id, id))
}

