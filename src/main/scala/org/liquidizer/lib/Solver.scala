package org.liquidizer.lib

import net.liftweb.mapper._
import net.liftweb.common.Logger
import org.liquidizer.model._

import VoteMap.EPS
import VoteMap.SWING_DECAY

class Solver(val room : Room) extends Logger {

  val WEIGHT_DECAY= room.decay.is / Tick.day

  /** In memory representation for the latest tick */
  class NomineeHead(val nominee : Votable) {
    var smooth = 0.0
    var tick : Option[Tick] = None

    /** Get the smooth average from the history */
    {
      val t0= Tick.now
      var t= t0
      val ts = Tick.getTimeSeries(nominee)
      if (!ts.isEmpty) tick=Some(ts.head)
      for (tick <- ts) {
	def h(time : Long) = Math.exp(- SWING_DECAY*(t0-time))
	smooth += tick.quote.value * (h(t) - h(tick.time.is))
	t= tick.time.is
      }
    }
    
    /** Return the current result */
    def result() = tick.map(_.quote).getOrElse(Quote(0.0, 0.0))
    
    /** Get the current decayed result */
    def result(time : Long) = tick.map { v =>
      v.quote * Math.exp(WEIGHT_DECAY*(v.time.is-time))}.getOrElse(Quote(0,0))

    /** Set the new Result */
    def update(time : Long, quote : Quote) = {
      // update smoothed value for trend indication
      val tickTime= tick.map { _.time.is }.getOrElse(time)
      val factor= Math.exp(SWING_DECAY*(tickTime-time))
      smooth= factor*smooth + (1-factor) * result(time).value

      if (!tick.exists( _.quote.distanceTo(quote)<EPS)) {
	// record a new tick every minute
	if (tickTime/Tick.min < time/Tick.min) tick=None
	// otherwise update the existing tick
	tick = Some(
	  tick.getOrElse { Tick.create.votable(nominee) }
	  .time(time)
	  .quote(quote))
	// persist result
	tick.get.save
      }
    }
  }

  /** In memory representation of user related data */
  class UserHead(val user : User, val nominee : Votable) {
    var vec = new VoteVector(user.id.is)
    var latestUpdate = 0L
    var latestVote = Vote
    .find(By(Vote.owner, user), OrderBy(Vote.date, Descending))
    .map { _.date.is }.getOrElse(0L)
    var active = true
    var factor = 1.0
    def weight(time : Long) = factor * Math.exp(WEIGHT_DECAY*(latestVote-time))
    def update(time : Long) = { latestVote = latestVote max time }
  }

  /** This class contains the code for solving the voting weight equation */
  var users= Map[Long, UserHead]()
  var nominees= Map[Long, NomineeHead]()
  var voteList= List[Vote]()
  var time = 0L

  /** Recompute all results following the latest votes */
  def recompute() : Unit = if (!voteList.isEmpty) {
    info("Recomputing room "+room+" with "+voteList.size+" updated votes")

    // iterative matrix solving
    preprocessVotes()
    sweep(100)

    // collect the results for each nominee
    var resultMap= Map[Long, Quote]()
    for (uHead <- users.values) {
      var active= false
      uHead.vec.votes.elements.foreach { case (i, e) =>
	val w= e.value * uHead.weight(time)
	if (w.abs > EPS) {
	  active= true
	  if (!resultMap.contains(i)) resultMap += i -> Quote(0,0)
	  resultMap.get(i).get += w
	}
      }
      // remember if this user actively participates
      uHead.active= active
    }
    // persist election results for queries
    for (nHead <- nominees.values if nHead.nominee.isQuery) {
      val quote= resultMap.get(nHead.nominee.query.is).getOrElse(Quote(0,0))
      setResult(nHead.nominee, quote) 
    }

    // normalize popularity to 1
    val denom= 1.0 / Math.sqrt(
      resultMap.values.foldLeft(1e-8) { (a,b) => a + b.volume * b.volume })

    // compute popularity as dot product with result vector
    for (uHead <- users.values) {
      val vec= uHead.vec
      var pop= Quote(0,0)
      if (uHead.active) {
        vec.votes.elements.foreach { case (i, e) =>
	  val w= e.value * uHead.weight(time)
	  resultMap.get(i).foreach { q => pop = pop + q * w }
	}
      }
      setResult(uHead.nominee, pop*denom)
    }
    voteList=Nil
  }

  /** Store the new result, trigger save to disk if necessary */
  def setResult(nominee : Votable, quote : Quote) = {
    getNomineeHead(nominee.id.is).update(time, quote)
  }

  /** Get a nominee head and ensure its presence */
  def getNomineeHead(nomId : Long) = {
    nominees.get(nomId).getOrElse {
      nominees+= nomId -> new NomineeHead(Votable.get(nomId).get)
      nominees.get(nomId).get
    }
  }

  /** Get a user head and ensure its presence */
  def getUserHead(userId : Long) = {
    users.get(userId).getOrElse {
      val user= User.find(By(User.id, userId)).get
      val nominee= Votable.get(List(user), room).head
      users+= userId -> new UserHead(user, nominee)
      users.get(userId).get
    }
  }

  /** process the list of votes in the voteList */
  def preprocessVotes() = {
    for (vote <- voteList) {
      if (vote.owner.obj.get.validated.is) {
	// update activity time for user
	val uHead= getUserHead(vote.owner.is)
	uHead.update(vote.date.is)
	// recount voting weight
	if (room.needsCode.is) {
	  uHead.factor= InviteCode.findAll(
	    By(InviteCode.room, room),
	    By(InviteCode.owner, uHead.user)).size
	}
	// delete old votes
	time = time max vote.date.is
	if (vote.weight.is==0 && uHead.latestVote > vote.date.is)
	  vote.delete_!
	else
	  getNomineeHead(vote.nominee.is)
      }
    }
  }

  /** Iterative solution of the users voting vectors */
  def sweep(maxIter : Int) : Unit = {
    var iterCount= 0

    while (!voteList.isEmpty && iterCount<maxIter) {
      var results = Map[Long, VoteVector]()
      
      // retreive affected votes for this room
      val votersList= voteList.map{ _.owner.is }.removeDuplicates
      val newVotes= Vote.findAll(ByList(Vote.owner, votersList),
			       ByList(Vote.nominee, nominees.keySet.toList))

      for (vote <- newVotes) {
	if (vote.owner.obj.get.validated.is) {
	  val userId= vote.owner.is
	  val nominee= getNomineeHead(vote.nominee.is).nominee

	  // prepare result vector for that user
	  val vec= results.get(userId).getOrElse {
	    results+= userId -> new VoteVector(userId)
	    results.get(userId).get
	  }

	  if (vote.weight.is != 0) {
	    if (nominee.isUser) {
	      // the vote is a delegation, mix in delegate's voting weights
	      users.get(nominee.user.is).foreach {
		uHead=> vec.addDelegate(vote.weight.is, uHead.vec)
	      }
	    } else {
	      // the vote is cast on a query
	      vec.addVote(vote.weight.is, nominee.query.is)
	    }
	  }
	}
      }
      updateVecs(results)
      iterCount+= 1
    }
  }

  def updateVecs(results : Map[Long, VoteVector]) = {
    var votersList= List[Long]()
    for (userId <- results.keySet) {
      // normalize voting weight
      val head= getUserHead(userId)
      val vec= results.get(userId).get
      vec.normalize();
      
      if (vec.distanceTo(head.vec) > EPS) {
	head.latestUpdate= time
	votersList ::= userId
      }
      // make the updated weights visible
      head.vec= vec
    }
    // determine affected delegating users
    val dlgts= Votable.get(votersList.map{ users.get(_).get.user }, room)
    voteList= Vote.findAll(ByList(Vote.nominee, dlgts.map{ _.id.is }))
  }
}

