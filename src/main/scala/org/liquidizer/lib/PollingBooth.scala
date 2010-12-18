package org.liquidizer.lib

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.common._

import _root_.org.liquidizer.model._


/** This is the only class that makes write access to the votes table */
object PollingBooth {
  
  private def getVote(owner : User, nominee : Votable) : Vote = {
    def allVotes() = Vote.findAll(
	By(Vote.owner, owner),
	nominee match {
	  case VotableQuery(query) => By(Vote.query, query)
	  case VotableUser(user) => By(Vote.user, user)
	},
	OrderBy(Vote.date, Descending))
    val votes = allVotes()
    if (votes.isEmpty) {
      // user has not voted on this issue yet
      vote(owner, nominee, 0)
      allVotes().first
    } 
    else 
      votes.first
  }

  def comment(owner : User, nominee : Votable, comment : String):Unit = {

    // find the latest vote object
    val vote = getVote(owner, nominee)

    // create and comment object
    val voteText=
      Comment.create
      .date(Tick.now)
      .vote(vote)
      .content(comment)
    voteText.save

    // attach comment to vote
    if (vote.comment.defined_?)
      vote.comment.obj.get.delete_!

    vote.comment(voteText)
    vote.save
    VoteCounter.register(vote)
  }
  
  def vote(owner : User, nominee : Votable, weight : Int) = {
    val vote=Vote.create
    .date(Tick.now)
    .owner(owner)
    .weight(weight)
    vote.setVotable(nominee)
    vote.save
    VoteCounter.register(vote)
  }

  def clearComments(owner : User) = {
    for(vote <- Vote.findAll(By(Vote.owner, owner))) {
      if (vote.comment.defined_?) {
	// unregister comment from in-memory map
	vote.comment.obj.get.content("")
	VoteCounter.registerComment(vote)
	// delete comment from database
	vote.comment.obj.get.delete_!
	vote.comment(Empty)
	vote.save
      }
    }
  }
}
