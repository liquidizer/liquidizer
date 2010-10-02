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
  val vec= new VoteVector(id)
  var votes : List[LinkedVote] = Nil
  val emos= mutable.Map.empty[User, Emotion]
}

class VoteMap {
  val voteMap= mutable.Map.empty[(User,Votable),LinkedVote]
  val users= mutable.Map.empty[User, UserHead]
  val nominees= mutable.Map.empty[Votable, NomineeHead]
  var dirty = false

  def id(user : User) = user.id.is.toInt
  def id(query : Query) = query.id.is.toInt

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
    link.preference= vote.weight.is
    dirty = true
  }
  
  def update(time : Long) : Unit = {
    for (i <- 1 to 20) sweep()

    for (head <- nominees) head._2.result= Quote(0,0)
    for (userHead <- users) {
      nominees.foreach { 
	case (VotableUser(delegate), nomineeHead) => 
	  nomineeHead.result = nomineeHead.result + 
	    Tick.toQuote(userHead._2.vec.getDelegationWeight(id(delegate)))
	case (VotableQuery(query), nomineeHead) => 
	  nomineeHead.result = nomineeHead.result + 
	    Tick.toQuote(userHead._2.vec.getVotingWeight(id(query)))
      }
    }
    for (head <- nominees) head._2.update(time)
    dirty = false
  }

  /** Iterative step to solve the equation system */
  def sweep() : Unit = synchronized {
    users.foreach { case (user,head) => 
      head.vec.clear()
      for (link <- head.votes) {
	link.nominee match {
	  case VotableQuery(query) => head.vec.addVote(link.preference, id(query))
	  case VotableUser(user) => head.vec.addDelegate(link.preference, users.get(user).get.vec)
	}
      }
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
	println("Query: "+nominee+" => "+getResult(nominee))
	head.votes.foreach {
	  vote => println("  "+vote.owner+":  "+vote.preference)
	}
      }}
  }
  
  def getVoteVector(user : User) : VoteVector = synchronized {
    users.get(user).map { _.vec }.getOrElse( new VoteVector(id(user)) )
  }

  def getResult(nominee : Votable) : Quote = synchronized {
    nominees.get(nominee).map{ _.result }.getOrElse(Quote(0,0))
  }

  def getWeight(user : User, nominee: Votable) : Double = nominee match {
    case VotableUser(delegate) => getVoteVector(user).getDelegationWeight(id(delegate))
    case VotableQuery(query) => getVoteVector(user).getVotingWeight(id(query))
  }

  def getPreference(user : User, nominee : Votable) : Int = {
    voteMap.get((user,nominee)).map { _.preference }.getOrElse(0)
  }

  def getSupporters(nominee : Votable) : List[LinkedVote] = {
      nominees.get(nominee).map { _.votes }.getOrElse(Nil)
  }

  def getVotes(user : User) : List[LinkedVote] = {
      users.get(user).map { _.votes }.getOrElse(Nil)
  }

  def getEmotion(user1 : User, user2 : User) : Option[Emotion] = {
    val key1= if (id(user1) < id(user2)) user1 else user2
    val key2= if (id(user1) < id(user2)) user2 else user1
    if (users.contains(key1) && users.contains(key2)) {
      val head1= users.get(key1).get
      val head2= users.get(key2).get

      if (!head1.emos.contains(key2)) head1.emos.put(key2, new Emotion())
      val emo= head1.emos.get(key2).get
      
      emo.valence.set(Tick.now, head1.vec.dotProd(head2.vec, false))
      emo.potency.set(Tick.now, head1.vec.dotProd(head2.vec, true))
      Some(emo)

    } else
      None
  }
}
