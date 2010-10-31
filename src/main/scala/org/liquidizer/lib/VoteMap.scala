package org.liquidizer.lib

import scala.collection.mutable

import _root_.net.liftweb.mapper._
import _root_.org.liquidizer.model._

class Emotion {
  val valence= new PoisonMemory(0.0)
  val potency= new PoisonMemory(0.0)
  val arousal= valence.swing

  override def toString() = valence.toString + " " + potency.value
}

class LinkedVote(val owner:User, val nominee:Votable) {
  
  var preference = 0
  override def toString():String= {
    "LinkedVote("+owner+" -> "+nominee+"  = "+preference+")"
  }
}

class NomineeHead {
  var result = Quote(0,0)
  val history = new TimeSeries()
  var votes  : List[LinkedVote] = Nil
  def update(time : Long) = history.add(time, result)
}

class UserHead(id : Int) {
  val vec = new VoteVector(id)
  var votes : List[LinkedVote] = Nil
  val emos = mutable.Map.empty[User, Emotion]
  var denom = 0
  var inflow = 1.0
}

class VoteMap {
  val voteMap= mutable.Map.empty[(User,Votable),LinkedVote]
  val users= mutable.Map.empty[User, UserHead]
  val nominees= mutable.Map.empty[Votable, NomineeHead]
  var dirty = false

  def id(user : User) = user.id.is.toInt
  def id(query : Query) = query.id.is.toInt
  def queryFromId(id : Int) = Query.getQuery(id.toLong)

  def put(vote:Vote) = {
    val owner= vote.owner.obj.get
    val nominee= vote.getVotable

    val link  = voteMap.get((owner,nominee)).getOrElse
    {
      // user has never voted for this nominee
      if (!nominees.contains(nominee)) nominees.put(nominee, new NomineeHead)
      if (!users.contains(owner)) users.put(owner, new UserHead(id(owner)))
      val nomineeHead = nominees.get(nominee).get
      val userHead = users.get(owner).get
      val newLink= new LinkedVote(owner, nominee)

      userHead.votes ::= newLink
      nomineeHead.votes ::= newLink

      // create entries in the vote map
      voteMap.put((owner,nominee), newLink)
      newLink
    }
    val newPref= vote.weight.is
    users.get(owner).get.denom += newPref.abs - link.preference.abs
    link.preference= newPref
    dirty = true
  }
  
  def update(time : Long) : Unit = {
    
    // iterative matrix solving
    for (i <- 1 to 20) sweep()

    // clear all nominee results
    for (head <- nominees) { head._2.result= Quote(0,0) }

    // collect the results for each nominee
    for (userHead <- users) {
      // sum all contributed voting weights to the results of queries
      nominees.foreach { 
	case (VotableQuery(query), nomineeHead) => 
	  val v=userHead._2.vec.getVotingWeight(id(query))
	  nomineeHead.result = nomineeHead.result + Tick.toQuote(v)
	case _ =>
      }
      // set the computed inflow as result for votable users
      val nominee= VotableUser(userHead._1)
      nominees.get(nominee).foreach { 
	_.result= Tick.toQuote(userHead._2.vec.getInflow())
      }
    }
    // include the new results in the time series
    for (head <- nominees) head._2.update(time)
    dirty = false
  }

  /** Iterative step to solve the equation system */
  def sweep() : Unit = synchronized {
    users.foreach { case (user,head) => 
      // reset the voting vector
      head.vec.clear()
      // vor each vote cast by the user update the voting vector
      for (link <- head.votes) {
	link.nominee match {
          case VotableUser(user) => 
            // the vote is a delegation, mix in the delegates voting weights
            val uHead= users.get(user)
            if (!uHead.isEmpty)
              head.vec.addDelegate(link.preference, uHead.get.vec)
	  case VotableQuery(query) => 
	    // the vote is cast on a query
	    head.vec.addVote(link.preference, id(query))
	  case _ =>
	}
      }
      // compute the inflow
      if (nominees.contains(VotableUser(user))) {
	for (link <- nominees.get(VotableUser(user)).get.votes) {
	  val uHead= users.get(link.owner).get
	  val denom= (uHead.denom.toDouble max 1.0)
	  head.vec.addSupporter(link.preference / denom, uHead.vec)
	}
      }
      // ensure the global voting weight constraint
      head.vec.normalize()
    }
  }

  private def voteFilter(link : LinkedVote) : Boolean = link.preference > 0
  
  /** Sort votes according to their absolute value */
  def sortVotes() : Unit = {
    nominees.foreach {
      case (key,head) => head.votes= head.votes.filter { voteFilter _}
    }
    users.foreach {
      case (key,head) => head.votes= head.votes.filter { voteFilter _}
    }
  }  
  
  /** dump to screen */
  def dump() = {
    users.foreach {
      case (user, head) => {
	println("User: "+user)
	head.votes.foreach {
	  vote => println("  "+vote)
	}
      }}
    println
    nominees.foreach {
      case (nominee, head) => {
	println("Query: "+nominee+" => "+getResult(nominee).get)
	head.votes.foreach {
	  vote => println("  "+vote.owner+":  "+vote.preference)
	}
      }}
  }
  
  def getVoteVector(user : User) : VoteVector = synchronized {
    users.get(user).map { _.vec }.getOrElse( new VoteVector(id(user)) )
  }

  /** get the global voting result for the nominee */
  def getResult(nominee : Votable) : Option[Quote] = synchronized {
    nominees.get(nominee).map{ _.result }
  }

  /** get the user's contribution to a vote on the nominee */
  def getWeight(user : User, nominee: Votable) : Double = synchronized {
    nominee match {
      case VotableQuery(query) => getVoteVector(user).getVotingWeight(id(query))
      case VotableUser(delegate) => getVoteVector(delegate).getSupportWeight(id(user))
    }
  }

  /** Get the integer preference number */
  def getPreference(user : User, nominee : Votable) : Int = {
    voteMap.get((user,nominee)).map { _.preference }.getOrElse(0)
  }

  /** Get all users who expressed a delegation */
  def getSupporters(nominee : Votable) : List[LinkedVote] = {
      nominees.get(nominee).map { _.votes }.getOrElse(Nil)
  }

  def getDenom(user : User) : Int =
    users.get(user).map { _.denom }.getOrElse(0)

  def getVotes(user : User) : List[LinkedVote] = {
      users.get(user).map { _.votes }.getOrElse(Nil)
  }

  def getEmotion(user1 : User, user2 : User, time : Long) : Option[Emotion] = {
    val key1= if (id(user1) < id(user2)) user1 else user2
    val key2= if (id(user1) < id(user2)) user2 else user1
    if (users.contains(key1) && users.contains(key2)) {
      val head1= users.get(key1).get
      val head2= users.get(key2).get

      if (!head1.emos.contains(key2)) head1.emos.put(key2, new Emotion())
      val emo= head1.emos.get(key2).get
      
      emo.valence.set(time, head1.vec.dotProd(head2.vec, false))
      emo.potency.set(time, head1.vec.dotProd(head2.vec, true))
      Some(emo)

    } else
      None
  }
}
