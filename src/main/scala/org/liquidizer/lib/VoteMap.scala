package org.liquidizer.lib

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable

import _root_.net.liftweb.mapper._
import _root_.org.liquidizer.model._

class LinkedVote(val owner:User, val nominee:Votable) extends VoteResult {
  
  val weight = new PoisonMemory(0.0)
  var nextUser : Option[LinkedVote] = None
  var nextNominee : Option[LinkedVote] = None
  
  def getUserVotes(): List[LinkedVote] = {
    this :: (if (nextNominee.isEmpty) Nil else nextNominee.get.getUserVotes())
  }

  def getNomineeVotes(): List[LinkedVote] = {
    this :: (if (nextUser.isEmpty) Nil else nextUser.get.getNomineeVotes())
  }
  
  override def toString():String= {
    "LinkedVote("+owner+" -> "+nominee+"  = "+weight+")"
  }
}

class NomineeHead {
  var result = Quote(0,0)
  val history = new TimeSeries()
  var votes  : Option[LinkedVote] = None

  def getVotes : List[LinkedVote] = 
    if (votes.isEmpty) Nil else votes.get.getNomineeVotes

  def setValue(time : Long, quote : Quote) = {
    history.add(time, quote)
    result= quote
  }
}

class UserHead() {
  val vec= new VoteVector()
  var votes : Option[LinkedVote] = None

  def getVotes : List[LinkedVote] =
    if (votes.isEmpty) Nil else votes.get.getUserVotes
}

class VoteMap {
  val voteMap= mutable.Map.empty[(User,Votable),LinkedVote]
  val users= mutable.Map.empty[User, UserHead]
  val nominees= mutable.Map.empty[Votable, NomineeHead]
  var dirty = false

  def put(vote:Vote) = {
    val owner= vote.owner.obj.get
    val nominee= vote.getVotable

    val link = voteMap.get((owner,nominee)).getOrElse
    {
      // user has never voted for this nominee
      val nomineeHead= nominees.get(nominee)
      .getOrElse(nominees.put(nominee, new NomineeHead()))
      val userHead= users.get(owner)
      .getOrElse(users.put(owner, new UserHead))
      val newLink= new LinkedVote(owner, nominee)

      newLink.nextNominee= userHead.votes
      userHead.votes= Some(newLink)

      newLink.nextUser= nomineeHead.votes
      nomineeHead.votes= Some(newLink)

      // create entries in the vote map
      voteMap.put((owner,nominee), newLink)
    }
    link.weight.set(vote.weight.is, vote.time.is)
    dirty = true
  }
  
  def update(time : Long) : Unit = {
    for (i <- 1 to 20) sweep()
    for (nominee <- nominees) nominee.result= Quote(0,0)
    for (user <- users) {
      nominees.foreach { 
	case (key, value) => value.result += Quote.toQuote(user.vec.get(key.id.is))
      }
    }
    for (nominee <- nominees) nominee.history.add(time, nominee.result)
    dirty = false
  }

  /** Iterative step to solve the equation system */
  def sweep() : Unit = synchronized {
    for (head <- users) {
      head.vec.clear()
      for (link <- user.getVotes) {
	link.nominee match {
	  case VotableQuery(query) => head.vec.addVote(link.weight, query.id.is)
	  case VotableUser(user) => head.vec.addDelegate(link.weight, users.get(user.id.is).get.vec)
	}
      }
      user.normalize()
    }
  }

  private def voteFilter(link : LinkedVote) : Boolean = {
    if (link.weight.smooth < 5e-3) {
      voteMap -= ((link.owner, link.nominee))
      false
    } else
      true
  }
  
  /** Sort votes according to their absolute value */
  def sortVotes() : Unit = {
    nominees.foreach {
      case (key,head) => 
	val list= head.getVotes
      .filter { voteFilter _ }
      .elements
      if (list.hasNext) {
	var elt= list.next
	head.votes= Some(elt)
	while (list.hasNext) {
	  val cur= list.next
	  elt.nextUser= Some(cur)
	  elt=cur
	}
	elt.nextUser= None
      } else {
	head.votes= None
      }
    }
    users.foreach {
      case (key,head) => 
	val list= head.getVotes
      .filter { voteFilter _ }
      .elements
      if (list.hasNext) {
	var elt= list.next
	head.votes= Some(elt)
	while (list.hasNext) {
	  val cur= list.next
	  elt.nextNominee= Some(cur)
	  elt=cur
	}
	elt.nextNominee= None
      } else {
	head.votes= None
      }
    }
  }  
  
  
  def dump() = {
    users.foreach {
      case (user, head) => {
	println("User: "+user+" / "+getDenom(user))
	head.getVotes.foreach {
	  vote => println("  "+vote)
	}
      }}
    println
    nominees.foreach {
      case (nominee, head) => {
	println("Query: "+nominee+" => "+getResult(nominee))
	head.getVotes.foreach {
	  vote => println("  "+vote.owner+":  "+vote.result)
	}
      }}
  }
  
  def getVotes(user : User, nominee : Votable) : VoteVector = synchronized {
    users.get(user).map { _.vec }.getOrElse( new VoteVector(user.id.is) ) }
  }

  def getResult(nominee : Votable) : Quote = synchronized {
    nominees.get(nominee).map{ _.result }.getOrElse(Quote(0,0))
  }

  def getSupporters(nominee : Votable) : List[LinkedVote] = {
    if (nominees.contains(nominee)) {
      nominees.get(nominee).get.getVotes
    } else {
      Nil
    }
  }

  def getVotes(user : User) : List[LinkedVote] = {
    if (users.contains(user)) {
      users.get(user).get.getVotes
    } else {
      Nil
    }
  }
}
