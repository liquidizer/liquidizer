package org.liquidizer.lib

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

import org.liquidizer.model._
import java.io._

/** The VoteCounter keeps track of all cast votes.
 *  It provides fast access to the latest results and statistical data.
 */
object VoteCounter {

  def init() = {
    // Refactor DB
    if (Tick.findAll.isEmpty) {
      // recompute history
      var time=0L
      Vote.findAll(OrderBy(Vote.date, Ascending)).foreach {
	vote =>
          // recompute results 
          if ((vote.date.is / Tick.day) > (time / Tick.day)) refresh()
	  time= vote.date.is
      }
      // delete historic votes
      var map= Set[(User,Votable)]()
      Vote.findAll(OrderBy(Vote.date, Descending)).foreach {
	vote =>
	  val key= (vote.owner.obj.get, vote.nominee.obj.get)
	    if (map.contains(key)) vote.delete_! else {
	      map+=key
	    }
      }
    }
    refresh()
  }
  
  /** recompte the results for all newly cast votes */
  def refresh() = synchronized {
    val t0= Tick.now
    VoteMap.update(t0)
    val t1= Tick.now
    println("Vote results update took "+(t1-t0)+" ms")
  }

  /** Maximum delegation weight. Used to compute emoticon size */
  def getMaxDelegation(user : User) : Int = {
    Vote.findAll(By(Vote.owner,user))
    .filter { _.nominee.obj.get.isUser }
    .map { _.weight.is }
    .foldLeft(0) { _ max _ }
  }

  /** Total voting result for a votable nominee */
  def getResult(nominee : Votable) : Quote = {
    VoteMap.getResult(nominee).getOrElse(Quote(0.0, 0.0))
  }

  /** Get a measure of how much the vote changed recently */
  def getSwing(nominee : Votable) : Double = {
    VoteMap.nominees.get(nominee).map { 
      head => (head.smooth - head.result.value).abs }.getOrElse(0.0)
  }

  def getPreference(user : User, nominee : Votable) : Int = {
    VoteMap.getPreference(user, nominee)
  }

  def getWeight(user : User, nominee : Votable) : Double = {
    VoteMap.getWeight(user, nominee)
  }
  
  def getAllVoters(nominee : Votable) : List[User] = {
    var list= Comment.findAll(By(Comment.nominee, nominee)).map { _.author.obj.get }
    // find voters recursively as voters and followers
    var proc= List(nominee)
    while (!proc.isEmpty) {
      val votes= proc.flatMap{ n => Vote.findAll(By(Vote.nominee, n)) }
      val next= votes
      .filter{ _.weight.is!=0 }.map{ _.owner.obj.get }
      .filter{ getWeight(_, nominee).abs > VoteMap.EPS }
      .removeDuplicates -- list
      proc=  next.map { VotableUser(_) }
      list ++= next
    }
    list
  }

  def getAllVotes(user : User) : List[Query] = {
    // extract list from voting vector
    var list= List[Query]()
    VoteMap.getVoteVector(user).foreach { vec =>
      for (i <- 0 to vec.votes.length-1) {
	val w= vec.getVotingWeight(i)
	if (w.abs > VoteMap.EPS)
	  VoteMap.queryFromId(i).foreach { list ::= _ }
      }
    }
    // include commentors
    val commentors= Comment.findAll(By(Comment.author, user))
    .map { _.nominee.obj.get }
    .filter{_.isQuery}.map { _.query.obj.get }
    // remove duplicates
    (list -- commentors) ++ commentors
  }

  def getActiveVoters(nominee : Votable) : List[User] = {
    Vote.findAll(By(Vote.nominee, nominee))
    .filter { _.weight!=0 }
    .map { _.owner.obj.get }
  }

  def getActiveVotes(user : User) : List[Votable] = {
    Vote.findAll(By(Vote.owner, user))
    .filter { _.weight!=0 }
    .map { _.nominee.obj.get }
  }

  def isDelegated(user : User, nominee : User) : Boolean = 
    user==nominee || getWeight(user,VotableUser(nominee))>1e-10

  def getTimeSeries(nominee : Votable) : List[Tick] = {
    val ts= Tick.getTimeSeries(nominee)
    Tick.compress(ts)
    ts
  }

  def getEmotion(user1 : User, user2 : User) : Option[Emotion] =
    VoteMap.getEmotion(user1, user2)

  def getSympathy(user1 : User, user2 : User) : Double =
    VoteMap.getCurrentWeight(user1) *
    VoteMap.getCurrentWeight(user2) *
    getEmotion(user1,user2).map{ _.valence.is }.getOrElse(0.0)

  def getCommentText(author : User, nominee : Votable) : String =
    getComment(author, nominee). map { _.content.is }.getOrElse("")

  def getCommentTime(author : User, nominee : Votable) : Long =
    Comment.get(author, nominee). map { _.date.is }.getOrElse(0L)

  def getComment(author : User, nominee : Votable) : Option[Comment] = 
    Comment.get(author, nominee)

  def getLatestComment(nominee : Votable) : Option[Comment] =
    Comment.find(By(Comment.nominee, nominee), 
		 OrderBy(Comment.date, Descending), MaxRows(1))

  def getLatestComment(user : User) : Option[Comment] =
    Comment.find(By(Comment.author, user), 
		 OrderBy(Comment.date, Descending), MaxRows(1))

}
