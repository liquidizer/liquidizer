package org.liquidizer.lib

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

import org.liquidizer.model._
import java.io._

/** The VoteCounter keeps track of all cast votes. It provides fast access to the latest 
 results and statistical data.
 */
object VoteCounter {

  val mapper= new VoteMapper
  var time = 0L
  
  class VoteMapper extends Actor with Logger {
    def act = {
      loop {
	react {
	  case 'PUSH =>  {
	    var senders = sender :: Nil
	    while (mailboxSize>0) receive {
	      case 'PUSH =>
	        senders ::= sender
	      case 'STOP =>
		exit()
	    }
	    updateFactors()
	    senders.foreach { _ ! 'FINISHED }
	  }
	  case 'STOP =>  exit()
	}
      }
    }

    def updateFactors() = {
      val t0= Tick.now
      VoteMap.update(t0)
      val t1= Tick.now
      info("Vote results update took "+(t1-t0)+" ms")
    }
  }
  
  def stop() = mapper ! 'STOP
  def refresh() = mapper !? 'PUSH

  def init() = {
    mapper.start
    
    // Refactor DB
    if (Tick.findAll.isEmpty) {
      // recompute history
      Vote.findAll(OrderBy(Vote.date, Ascending)).foreach {
	vote =>
          // recompute results 
          if ((vote.date.is / Tick.h) > (time / Tick.h)) mapper.updateFactors()
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
    mapper.updateFactors
    refresh()
  }
  
  def getDelegationScale(user : User) : (Double, Int)= {
    var scale=0.0
    var pref=0
    getActiveVotes(user)
    .map { 
      case u @ VotableUser(_) => 
	scale= scale max getWeight(user, u)
        pref= pref max getPreference(user, u)
      case _ =>  }
    (scale, pref)
  }

  /** Total voting result for a votable nominee */
  def getResult(nominee : Votable) : Quote = {
    VoteMap.getResult(nominee).getOrElse(Quote(0.0, 0.0))
  }

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
  
  //TODO deprecate
  def getAllVoters(query : Query) : List[User] = getAllVoters(VotableQuery(query))

  def getAllVoters(nominee : Votable) : List[User] = {
    var list= Comment.findAll(By(Comment.nominee, nominee)).map { _.author.obj.get }
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
    var list= Comment.findAll(By(Comment.author, user)).map { _.nominee.obj.get }
    var proc= List(user)
    while (!proc.isEmpty) {
      val votes= proc.flatMap{ u => Vote.findAll(By(Vote.owner, u)) }
      val next= votes
      .filter{ _.weight.is!=0 }.map{ _.nominee.obj.get }
      .filter{ getWeight(user, _).abs > VoteMap.EPS }
      .removeDuplicates -- list
      proc=  next.filter{ _.isUser }.map { _.user.obj.get }
      list ++= next
    }
    //TODO return list of votables
    list.filter{ _.isQuery }.map{ _.query.obj.get }
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
