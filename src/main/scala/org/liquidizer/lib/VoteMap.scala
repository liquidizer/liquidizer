package org.liquidizer.lib

import net.liftweb.mapper._
import org.liquidizer.model._

object VoteMap {
  val DECAY= 5e-10

  /** In memory representation for the latest tick */
  class NomineeHead(val nominee : Votable) {
    var smooth = 0.0
    var tick : Option[Tick] = None

    /** Get the smooth average from the history */
    {
      val t0= Tick.now
      var t= t0
      for (tick <- Tick.getTimeSeries(nominee)) {
	smooth += tick.quote.value * 
	(Math.exp(- DECAY*(t0-t)) - Math.exp(-DECAY*(t0-tick.time.is)))
	t= tick.time.is
      }
    }
    
    /** Return the current result */
    def result() = tick.map(_.quote).getOrElse(Quote(0.0, 0.0))
    
    /** Set the new Result */
    def update(time : Long, quote : Quote) = {
      // record a new tick every minute
      if (tick.exists { _.time.is/Tick.min < time/Tick.min}) tick=None
      // otherwise update the existing tick
      tick = Some(
	tick.getOrElse { Tick.create.votable(nominee) }
	.time(time)
	.quote(quote))
      // persist result
      tick.get.save
    }
  }

  class UserHead(val user : User) {
    var vec = new VoteVector(id(user))
    var latestUpdate = 0L
    var latestVote = Vote
      .find(By(Vote.owner, user), OrderBy(Vote.date, Descending))
      .map { _.date.is }.getOrElse(0L)
    def update(time : Long) = { latestVote = latestVote max time }
  }

  var users= Map[User, UserHead]()
  var nominees= Map[Votable, NomineeHead]()
  var lastUpdate = 0L

  def id(user : User) = user.id.is.toInt
  def id(query : Query) = query.id.is.toInt
  def queryFromId(id : Int) = Query.getQuery(id.toLong)

  def update(time : Long) : Unit = {
    // iterative matrix solving
    sweep(time, 1000, 1e-4)

    // collect the results for each nominee
    var resultMap= Map[Int, Quote]()
    for (user <- users) {
      val votes= user._2.vec.votes
      for (i <- 0 to votes.length-1) {
	if (votes(i).abs > 1e-8) {
	  if (!resultMap.contains(i)) resultMap += i -> Quote(0,0)
	  resultMap.get(i).get += votes(i)
	}
      }
    }

    // set election results
    resultMap.foreach { case (i,quote) => 
      setResult(time, VotableQuery(queryFromId(i).get), quote) }

    // compute popularity
    for (user <- users) {
      val votes= user._2.vec.votes
      var pop= Quote(0,0)
      for (i <- 0 to votes.length-1) {
	if (votes(i).abs > 1e-8) pop = pop + (resultMap.get(i).get * votes(i))
      }
      setResult(time, VotableUser(user._1), pop)
    }
  }

  def setResult(time : Long, nominee : Votable, quote : Quote) = {
    nominees.get(nominee).getOrElse {
      val head= new NomineeHead(nominee)
      nominees+= (nominee -> head)
      head
    }.update(time, quote)
  }

  /** Iterative step to solve the equation system */
  def sweep(time : Long, maxIter : Int, eps : Double) : Unit = {
    var votes = Vote.findAll(By_>(Vote.date, lastUpdate))

    // update last vote counter
    for (vote <- votes) {
      val user= vote.owner.obj.get
      if (!users.contains(user))
	users += (user -> new UserHead(user))
      users.get(user).get.update(vote.date.is)
    }

    var list= votes.map {_.owner.obj.get}.removeDuplicates
    var iterCount= maxIter
    
    // repeat until convergence is reached
    while (!list.isEmpty && iterCount<maxIter) {
      var nextList= List[User]()
      for (user <- list) {
	val head= users.get(user).get

	// reset the voting vector
	val decay= Math.exp(VoteMap.DECAY * (head.latestVote - time))
	val vec= new VoteVector(head.vec)
	vec.clear(decay)
	
	// vor each vote cast by the user update the voting vector
	for (vote <- Vote.findAll(By(Vote.owner, user)).filter(_.weight!=0)) {
	  vote.nominee.obj.get match {
            case VotableUser(user) => 
              // the vote is a delegation, mix in the delegates voting weights
		val uHead= users.get(user)
               if (!uHead.isEmpty)
                 vec.addDelegate(vote.weight.is, uHead.get.vec)
	    case VotableQuery(query) => 
	      // the vote is cast on a query
	      vec.addVote(vote.weight.is, id(query))
	    case _ =>
	  }
	}

	// ensure the global voting weight constrain
	vec.normalize()
	if (vec.distanceTo(head.vec) > eps) {
	  head.latestUpdate= time
	  // process followers
	  val follow= Vote.findAll(By(Vote.nominee, VotableUser(user)))
	  nextList ++= follow.filter { _.weight.is!=0 }.map { _.owner.obj.get }
	}
	head.vec= vec
	iterCount+= 1
      }
      list= nextList.removeDuplicates
    }
    lastUpdate= time
  }

  /** Get the VoteVector */
  def getVoteVector(user : User) : Option[VoteVector] =
    users.get(user).map { _.vec }

  /** get the global voting result for the nominee */
  def getResult(nominee : Votable) : Option[Quote] = {
    nominees.get(nominee).map{ _.result }
  }

  /** get the user's contribution to a vote on the nominee */
  def getWeight(user : User, nominee: Votable) : Double = {
    nominee match {
      case VotableQuery(query) =>
	getVoteVector(user).map { _.getVotingWeight(id(query)) }.getOrElse(0.)
      case VotableUser(delegate) => 
	getVoteVector(delegate).map { _.getSupportWeight(id(user)) }.getOrElse(0.)
    }
  }

  /** Get the integer preference number */
  def getPreference(user : User, nominee : Votable) : Int = 
    Vote.get(user,nominee).map { _.weight.is }.getOrElse(0)

  def isActive(user : User) : Boolean =
    user.validated && users.get(user).exists { _.vec.getActiveWeight>0 }

  def getEmotion(user1 : User, user2 : User, time : Long) : Option[Emotion] = {
    if (isActive(user1) && isActive(user2)) {
      val emo= Emotion.get(user1, user2)

      // decay of emotional arousal
      emo.update(time, VoteMap.DECAY)

      // check if emotion needs to be updated
      val head1= users.get(user1).get
      val head2= users.get(user2).get

      // compute new emotion
      if (head1.latestUpdate>time || head2.latestUpdate>time) {
	emo.valence(head1.vec.dotProd(head2.vec, false))
	emo.potency(head1.vec.dotProd(head2.vec, true))
	emo.update(time, VoteMap.DECAY)
	emo.save
      }

      Some(emo)
    } else
      None
  }
}
