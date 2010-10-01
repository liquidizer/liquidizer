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
  var lastPushTime = 0L
  
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
	    updateFactors()
	    senders.foreach { _ ! 'FINISHED }
	  }
	  case 'PUSH => push(Tick.now); sender ! 'FINISHED
	  case 'STOP =>  exit()
	}
      }
    }
    def push(vote:Vote) : Unit = {
      val time= vote.date.is
      voteMap.put(vote)
//      println(" -> "+vote)
    }

    def updateFactors() = {
      if (voteMap.dirty) {
	val t0= Tick.now
	voteMap.update()
	val t1= Tick.now
	Log.info("Vote results update took "+(t1-t0)+" ms")
      }}

  
  def stop() = mapper ! 'STOP
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
  
  def getWeight(user : Option[User]) : Int = {
    if (user.isEmpty) 0 else getWeight(user.get)
  }

  def getWeight(owner : User) : Int = {
    voteMap.getDenom(owner)
  }
  
  def getResult(nominee : Votable) : Quote = {
    voteMap.getResult(nominee)
  }
  
  def getResult(query : Query) : Quote = getResult(VotableQuery(query))
  def getResult(user : User) : Quote = getResult(VotableUser(user))
  
  def getWeight(user : Option[User], nominee : Votable) : Int = {
    if (user.isEmpty) 0 else getWeight(user.get, nominee)
  }

  def getWeight(user : User, nominee : Votable) : Int = {
    voteMap.voteMap.get(user, nominee).map { _.weight }.getOrElse(0)
  }

  def getBlurredVolume(user : User, nominee : Votable) : Double = {
    voteMap.voteMap.get(user,nominee).map { 
      link => link.expVolume
    } 
    .getOrElse(0.0)
  }

  def getResult(user : User, nominee : Votable) : Double = 
    voteMap.getResult(user,nominee)

  def getComment(author : User, nominee : Votable) : Option[String] = {
    comments.get(author, nominee). map { _.content.is }
  }

  def getCommentTime(author : User, nominee : Votable) : Long = {
    comments.get(author, nominee). map { _.date.is }.getOrElse( 0L )
  }

  def getSwing(nominee : Votable) : Double = 
    voteMap.getSwingFactor(nominee)
  def getSwing(user: User, nominee : Votable) : Double = 
    voteMap.getSwingFactor(user, nominee)
  
  def getSupporters(nominee : Votable, includeVoid : Boolean) : List[User] = {
    voteMap.getSupporters(nominee)
    .filter { includeVoid || _.weight!=0 }
    .map { link => link.owner }
  }

  def getVotes(user : User, includeVoid : Boolean) : List[Votable] = {
    voteMap.getVotes(user)
    .filter { includeVoid || _.weight!=0 }
    .map { link => link.nominee }
  }

  def isDelegated(user : User, nominee : User) : Boolean = 
    isDelegated(user, nominee, mutable.Set.empty[User])

  def isDelegated(user : User, nominee : User, visited : mutable.Set[User]) 
  : Boolean = {
    if (user==nominee) {
      true
    } 
    else if (visited.contains(nominee)) {
      false
    }
    else {
      visited += nominee
      var result= false;
      getSupporters(VotableUser(nominee), false).foreach {
	supporter => result ||= isDelegated(user, supporter, visited)
      }
      result
    }
  }
}
