package org.liquidizer.lib

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.common._

import _root_.org.liquidizer.model._


/** This is the only class that makes write access to the votes table */
object PollingBooth {

  /** create a comment for a given user, for a given nominee */
  def comment(author : User, nominee : Votable, text : String):Unit = {
    val obj= Comment.get(author, nominee)
    if (text.trim.isEmpty) {
      obj.map { _.delete_! }
    } else {
      obj.getOrElse(Comment.create)
      .date(Tick.now)
      .author(author)
      .nominee(nominee)
      .content(text).save
    }

  }

  def vote(owner : User, nominee : Votable, weight : Int) = {
    val vote = Vote.get(owner, nominee).getOrElse {
      Vote.create.owner(owner).nominee(nominee)
    }.date(Tick.now).weight(weight)
    vote.save
    VoteCounter.refresh
  }

  def clearComments(owner : User) = {
    for(comment <- Comment.findAll(By(Comment.author, owner))) {
      comment.delete_!
    }
  }
}
