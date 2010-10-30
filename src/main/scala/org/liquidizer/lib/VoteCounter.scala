package org.liquidizer.lib

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

import org.liquidizer.model._


class CommentMap {
  val map= mutable.Map.empty[(User, Votable), Comment]
  
  def add(vote:Vote) ={
    if (vote.comment.defined_?) {
      val key= (vote.owner.obj.get, vote.getVotable)
      val comment= vote.comment.obj.get
      if (comment.content.is.trim=="")
	map -= key
      else
	map.put(key, comment)
    }
  }
  
  def get(user : User, nominee : Votable) : Option[Comment] = {
    map.get((user, nominee))
  }
}

/** The VoteCounter keeps track of all cast votes. It provides fast access to the latest 
 results and feeds snapshots to the QuoteHistory object for historic data analysis.
 */
object VoteCounter {

  private val voteMap= new VoteMap
  val comments= new CommentMap
  val mapper= new VoteMapper
  var time = 0L
  
  class VoteMapper extends Actor {
    def act = {
      loop {
	react {
	  case vote:Vote =>  {
	    push(vote)
	    var senders = sender :: Nil
	    while (mailboxSize>0) receive {
	      case vote:Vote => 
		push(vote)
	        senders ::= sender
	      case 'PUSH =>
	        senders ::= sender
	      case 'STOP =>
		exit()
	    }
	    updateFactors()
	    senders.foreach { _ ! 'FINISHED }
	  }
	  case 'STOP =>  exit()
	  case 'PUSH =>  sender ! 'FINISHED
	}
      }
    }

    def push(vote : Vote) = {
      time= vote.date.is
      voteMap.put(vote)
    }

    def updateFactors() = {
      if (voteMap.dirty) {
	val t0= Tick.now
	voteMap.update(time)
	val t1= Tick.now
	Log.info("Vote results update took "+(t1-t0)+" ms")
      }
    }
  }
  
  def stop() = {
    println("stopping") 
    mapper ! 'STOP
  }
  def refresh() = mapper !? 'PUSH

  def register(vote : Vote) {
    comments.add(vote)
    mapper !? vote
  }
  
  def init() = {
    mapper.start
    Vote.findAll(OrderBy(Vote.date, Ascending)).foreach {
      vote =>
	comments.add(vote)
        // recompute results 
        if ((vote.date.is / Tick.h) > (time / Tick.h)) mapper.updateFactors()
        if ((vote.date.is / Tick.day) > (time / Tick.day)) {
	  for (user1 <- voteMap.users.keySet)
	    for (user2 <- voteMap.users.keySet)
	      voteMap.getEmotion(user1, user2, time)
	}
        mapper.push(vote)
    }
    mapper.updateFactors()
    refresh()
  }
  
  
  def dump:Unit = {
    voteMap.dump()
  }
  
  def getDelegationInflow(user : User) : Double = {
    voteMap.getResult(VotableUser(user)).map { _.pro }.getOrElse {
      if (getMaxPref(user)>0) 1.00 else 0.00 
    }
  }

  def getDelegationOutflow(user : User) : Double = {
    voteMap synchronized {
      voteMap.users.get(user).map { uHead => 
	uHead.votes.foldLeft (0.0) { (sum,link) => 
	  link.nominee match {
	  case VotableUser(user) => sum + link.preference.toDouble / uHead.denom
	  case _ => sum
	}} *
	getDelegationInflow(user)
      }.getOrElse(0.0)
    }
  }

  def getResult(nominee : Votable) : Quote = {
    voteMap.getResult(nominee).getOrElse(Quote(0.0, 0.0))
  }

  def getMaxPref(user : User) : Int = 
    voteMap.users.get(user).map { 
      _.votes.foldLeft(0) { (a,b) =>  a max (b.preference.abs) }
    }.getOrElse(0)

  def getPreference(user : User, nominee : Votable) : Int = {
    voteMap.getPreference(user, nominee)
  }

  def getWeight(user : User, nominee : Votable) : Double = {
    voteMap.getWeight(user, nominee)
  }

  def getComment(author : User, nominee : Votable) : Option[String] = {
    comments.get(author, nominee). map { _.content.is }
  }

  def getCommentTime(author : User, nominee : Votable) : Long = {
    comments.get(author, nominee). map { _.date.is }.getOrElse( 0L )
  }

  def getAllVoters(query : Query) : List[User] = {
    val id= voteMap.id(query)
    voteMap synchronized {
      voteMap.users
      .filter { 
	case (u,head) => 
	  Math.abs(head.vec.getVotingWeight(id)) >= 5e-3 || 
	  !getComment(u, VotableQuery(query)).isEmpty
      }
      .map { case (u,head) => u }
      .toList
    }
  }

  def getAllVotes(user : User) : List[Query] = {
    voteMap synchronized {
      voteMap.nominees
      .flatMap {
	case (n @ VotableQuery(query), head) 
	  if Math.abs(getWeight(user,n)) >= 5e-3 ||
	     !getComment(user, n).isEmpty => List(query)
	case _ => Nil
      }.toList
    }
  }

  def getActiveVoters(nominee : Votable) : List[User] = {
    voteMap.getSupporters(nominee)
    .filter { _.preference!=0 }
    .map { _.owner }
  }

  def getActiveVotes(user : User) : List[Votable] = {
    voteMap.getVotes(user)
    .filter { _.preference!=0 }
    .map { _.nominee }
  }

  def isDelegated(user : User, nominee : User) : Boolean = 
    getWeight(user,VotableUser(nominee))>1e-10

  def getTimeSeries(nominee : Votable) : List[Tick[Quote]] =
    voteMap.nominees.get(nominee).map { _.history.getAll }.getOrElse(Nil)

  def getEmotion(user1 : User, user2 : User) : Option[Emotion] = {
    voteMap.getEmotion(user1, user2, Tick.now)
  }
}
