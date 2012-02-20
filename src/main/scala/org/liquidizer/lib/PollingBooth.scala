package org.liquidizer.lib

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.common._

import _root_.org.liquidizer.model._


/** This is the only class that makes write access to the votes table */
object PollingBooth {
  /** time of the latest vote */
  var latestVote= 0L

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

  /** A user votes on a nominee with given preference weight */
  def vote(owner : User, nominee : Votable, weight : Int) = synchronized {
    latestVote= Math.max(latestVote+1, Tick.now)
    val vote = Vote.get(owner, nominee).getOrElse {
      Vote.create.owner(owner).nominee(nominee)
    }.date(latestVote).weight(weight)
    vote.save
  }

  /** Refresh the users voting weight */
  def activate(user : User, room : Option[Room], code : String) : Boolean = {
    if (!room.isEmpty) {
      var n= Votable.find(By(Votable.user, user), 
			  By(Votable.room, room.get));
      if (room.get.needsCode.is) {
	if (code.isEmpty && n.isEmpty)
	  return false
	if (!code.isEmpty) {
	  val codeItem= InviteCode.find(By(InviteCode.room, room.get),
					By(InviteCode.code, code))
	  if (codeItem.isEmpty) {
	    if (n.isEmpty)
	      return false
	  } else {
	    if (codeItem.get.owner.is!=user.id.is) {
	      if (!codeItem.get.owner.isEmpty)
		return false;
	      codeItem.get.owner(user);
	      codeItem.get.save
	    }
	  }
	}
      }
      if (n.isEmpty)
	n= Some(Votable.create.user(user).room(room.get).saveMe)
      PollingBooth.vote(user, n.get, 0)
    }
    return true;
  }

  /** Clear all comments given by a user */
  def clearComments(owner : User) = {
    for(comment <- Comment.findAll(By(Comment.author, owner))) {
      comment.delete_!
    }
  }

  /** Clear all active votes cast by a user */
  def clearVotes(user : User) = {
    val votes= Vote.findAll(By(Vote.owner, user))
    votes.foreach { v => vote(user, v.nominee.obj.get, 0) }
    VoteMap.refresh()
  }

  /** Delete nominee */ 
  def deleteVotable(nominee : Votable) = {
    for (vote <- Vote.findAll(By(Vote.nominee, nominee)))
      vote.delete_!
    for (comment <- Comment.findAll(By(Comment.nominee, nominee)))
      comment.delete_!
    nominee.delete_!
    nominee match {
      case VotableQuery(query) => query.delete_!
      case _ =>
    }
  }
}
