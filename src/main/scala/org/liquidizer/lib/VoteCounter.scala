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
	receive {
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
	    updateFactors(vote.date.is)
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

    def updateFactors(time : Long) = {
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
        mapper ! vote

    }
    refresh()
  }
  
  
  def dump:Unit = {
    voteMap.dump()
  }
  
  def getDelegationWeight(user : User) : Double = {
    voteMap synchronized {
      val id= user.id.is
      voteMap.users.map { 
	case (key,value) => voteMap.getVoteVector(key).getDelegationWeight(voteMap.id(user)) 
      }.reduceLeft(_+_)
    }
  }

  def getResult(query : Query) : Quote = getResult(VotableQuery(query))
  def getResult(user : User) : Quote = getResult(VotableUser(user))
  def getResult(nominee : Votable) : Quote = {
    voteMap.getResult(nominee)
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

  def getSupporters(nominee : Votable, includeVoid : Boolean) : List[User] = {
    voteMap.getSupporters(nominee)
    .filter { includeVoid || _.preference!=0 }
    .map { link => link.owner }
  }

  def getVotes(user : User, includeVoid : Boolean) : List[Votable] = {
    voteMap.getVotes(user)
    .filter { includeVoid || _.preference!=0 }
    .map { link => link.nominee }
  }

  def isDelegated(user : User, nominee : User) : Boolean = 
    getWeight(user,VotableUser(nominee))>1e-10

  def getTimeSeries(nominee : Votable) : List[Tick[Quote]] =
    voteMap.nominees.get(nominee).map { _.history.getAll }.getOrElse(Nil)

  def getEmotion(user1 : User, user2 : User) : Option[Emotion] = {
    voteMap.getEmotion(user1, user2)
  }
}
