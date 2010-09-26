package org.liquidizer.lib

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable

import _root_.net.liftweb.mapper._

import org.liquidizer.model._

abstract class VoteResult {
  var expVolume : Double = 0.0
  var expResult : Double = 0.0
}

class LinkedVote(val owner:User, val nominee:Votable) extends VoteResult {
  
  var weight : Int = 0
  var result : Double = 0.0
  var savedResult : Double = 0.0
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

class NomineeHead extends VoteResult {
  var result : Quote = Quote(0,0)
  var votes  : Option[LinkedVote] = None
  def getVotes : List[LinkedVote] = 
    if (votes.isEmpty) Nil else votes.get.getNomineeVotes
}

class UserHead() {
  var denom : Int = 0
  var votes : Option[LinkedVote] = None
  def getVotes : List[LinkedVote] =
    if (votes.isEmpty) Nil else votes.get.getUserVotes
}

class VoteMap {
  val voteMap= mutable.Map.empty[(User,Votable),LinkedVote]
  val users= mutable.Map.empty[User, UserHead]
  val nominees= mutable.Map.empty[Votable, NomineeHead]

  var dirty= false
  var lastUpdateTime= 0L
  var latestVoteTime= 0L
  
  def put(vote:Vote) {
    val owner= vote.owner.obj.get
    val nominee= vote.getVotable
    nominee match {
      case VotableUser(delegate) =>
	if (delegate == owner)
	  return
      if (!users.contains(delegate))
	return
      case _ =>
    }
    // ensure existes in nominee map
    if (!nominees.contains(nominee))
      nominees.put(nominee, new NomineeHead())
    if (!nominees.contains(VotableUser(owner)))
      nominees.put(VotableUser(owner), new NomineeHead())
    if (!users.contains(owner))
      users.put(owner, new UserHead())

    if (!voteMap.contains((owner, nominee))) {
      // user has never voted for this nominee
      val nomineeHead= nominees.get(nominee).get
      val userHead= users.get(owner).get
      val link= new LinkedVote(owner, nominee)

      link.nextNominee= userHead.votes
      userHead.votes= Some(link)

      link.nextUser= nomineeHead.votes
      nomineeHead.votes= Some(link)

      // create entries in the vote map
      voteMap.put((owner,nominee), link)
    }
    latestVoteTime= vote.date.is
    inscribe(voteMap.get((owner,nominee)).get, vote.weight.is)
  }
  
  /** register vote result */
  private def inscribe(link : LinkedVote, newWeight : Int):Unit = {
    val head= users.get(link.owner).get
    head.denom= head.denom - Math.abs(link.weight) + Math.abs(newWeight)
    link.weight= newWeight
    dirty = true
  }
  
  // create a compact and immutable map of result data for later extractions of time series
  def getUpdateMap() : Map[Any, Quote] = {
    collection.immutable.ListMap[Any,Quote](
      // election results for nominees
      nominees.map { case (key, value) => key -> value.result }.toSeq
      ++
      // user voting history
      voteMap
      .filter { case (key, link) => link.savedResult != link.result }
      .map { case (key, link) => 
	link.savedResult = link.result
	key -> Tick.toQuote(link.result) }
      :_*
    )
  }
  
  /** complete update of all factors, including cyclic delegations */
  def updateFactors() : Unit = synchronized {
    decay()
    voteMap.values.foreach { _.result = 0. }
    nominees.values.foreach { _.result= Quote(0,0) }
    users.keys.foreach {
      user =>
        val quote= updateUserWeights(user, List())
      nominees.get(VotableUser(user)).get.result= quote
    }
    users.keys.foreach {
      user => 
	val factor= 1.0/(1.0- getResult(VotableUser(user)).contra)
      if (factor<1e10)
	updateQueryWeights(factor, user, List())
    }
    users.keys.foreach { 
      user =>
	val nominee= VotableUser(user)
      nominees.get(nominee).foreach {
	head => head.result= Tick.toQuote(head.result.pro-1)
      }
    }
    sortVotes()
    dirty= false
  }

  private def voteFilter(link : LinkedVote) : Boolean = {
    if (link.weight==0 && 
	link.expVolume<5e-3 &&
        link.savedResult== link.result) {
      voteMap -= ((link.owner, link.nominee))
      false
    } else
      true
  }
  
  /** compute the exponentially weighted moving average result */
  def decay() : Unit = {
    val DECAY = 1e-9
    val dt= latestVoteTime - lastUpdateTime
    val factor= Math.exp(-DECAY*dt)
    lastUpdateTime= latestVoteTime

    // comute moving average for user votes
    voteMap.values.foreach {
      link => 
	link.expVolume = factor*link.expVolume + (1-factor)*Math.abs(link.result)
	link.expResult = factor*link.expResult + (1-factor)*link.result
    }
    // compute decay for poll results
    nominees.values.foreach {
      h =>
	h.expResult = factor*h.expResult + (1-factor)*h.result.value
	h.expVolume = factor*h.expVolume + (1-factor)*h.result.volume
    }
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
  
  /** update all transitive weights generated by vote delegation */
  private def updateUserWeights(nominee : User, buf : List[User]) : Quote = {
    
    val self= VotableUser(nominee)
    val cycle= buf.contains(nominee)
    if (cycle) {
      Quote(0., if (nominee==buf.last) 1. else 0.)
    } else {
      var quote= Quote(1.,0.)
      if (nominees.contains(self)) {
	nominees.get(self).get.getVotes.foreach {
	  link =>
	    val denom= getDenom(link.owner)
	  if (denom>0) {
	    val factor= (link.weight.toDouble/denom)
	      val update= updateUserWeights(link.owner, nominee :: buf) * factor
	    quote = quote +  update
	    if (buf.isEmpty)
	      link.result += update.pro
	  }
	}
      }
      quote
    }
  }
  
  /** Compute all vote results */
  private def updateQueryWeights(factor : Double, votee : User, buf : List[User]):Unit = {
    val cycle= buf.contains(votee)
    if (!cycle) {
      val denom = getDenom(votee)
      if (denom > 0 && factor>1e-5) {
	users.get(votee).get.getVotes.foreach {
	  link =>
	    link.nominee match {
	      case VotableQuery (query) =>
		val head= nominees.get(link.nominee).get
	      val oldQuote = head.result
	      val update = Tick.toQuote(factor*link.weight/denom)
	      link.result += update.value 
	      head.result= oldQuote + update
	      case VotableUser (user) =>
	    	updateQueryWeights(factor*link.weight/denom, user, votee :: buf)
	    }
	}
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
  
  def getDenom(user:User) : Int = {
    users.get(user).map { _.denom }.getOrElse(0)
  }
  
  def getResult(user : User, nominee : Votable) : Double = synchronized {
    voteMap.get(user, nominee).map { _.result }.getOrElse(0.)
  }

  def getResult(nominee : Votable) : Quote = synchronized {
    nominees.get(nominee).map{ _.result }.getOrElse(Quote(0,0))
  }

  def getSwingFactor(nominee : Votable) : Double = synchronized {
    nominees.get(nominee).map { 
      h => Math.abs(h.expResult - h.result.value) +
      0.8*(h.result.volume - h.expVolume)
    }.getOrElse(0)
  }

  def getSwingFactor(user : User, nominee : Votable) : Double = synchronized {
    voteMap.get(user,nominee).map { 
      link => Math.abs(link.expResult - link.result)
    }.getOrElse(0)
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
