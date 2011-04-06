package org.liquidizer.lib

import net.liftweb.mapper._
import org.liquidizer.model._

import VoteMap.EPS
import VoteMap.WEIGHT_DECAY
import VoteMap.SWING_DECAY

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
  def weight(time : Long) = Math.exp(WEIGHT_DECAY*(latestVote-time))
  def update(time : Long) = { latestVote = latestVote max time }
}

/** This class contains the code for solving the voting weight equation */
class Solver(val room : Room) {
  var users= Map[Long, UserHead]()
  var nominees= Map[Long, NomineeHead]()
  var votersList : List[Long] = Nil
  var time = 0L

  /** Recompute all results following the latest votes */
  def recompute() : Unit = if (!votersList.isEmpty) {
    // iterative matrix solving
    sweep(100, 1e-4)

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
    // persist election results
    resultMap.foreach { case (i, quote) => setResult(i, quote) }

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
      setResult(uHead.nominee.id.is, pop*denom)
    }
  }

  /** Store the new result, trigger save to disk if necessary */
  def setResult(nomId : Long, quote : Quote) = {
    getNomineeHead(nomId).update(time, quote)
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

  /** Iterative solution of the users voting vectors */
  def sweep(maxIter : Int, eps : Double) : Unit = {
    var results = Map[Long, VoteVector]()
    var iterCount= 0

    while (!votersList.isEmpty && iterCount<maxIter) {
      votersList= votersList.removeDuplicates
      println(iterCount+": VOTERS= "+votersList)
      val newVotes= Vote.findAll(ByList(Vote.owner, votersList))

      for (vote <- newVotes) {
	val userId= vote.owner.is
	users.get(userId).foreach { uHead =>
	  uHead.update(vote.date.is)
	  if (vote.weight.is==0 && uHead.latestVote > vote.date.is)
	    vote.delete_!
	}
	time= time max vote.date.is

	val vec= results.get(userId).getOrElse {
	  results+= userId -> new VoteVector(userId)
	  results.get(userId).get
	}

	val nominee= getNomineeHead(vote.nominee.is).nominee
	if (nominee.isUser) {
	  // the vote is a delegation, mix in delegate's voting weights
	  users.get(userId).foreach {
            uHead=> vec.addDelegate(vote.weight.is, uHead.vec)
	  }
	} else {
	  // the vote is cast on a query
	  vec.addVote(vote.weight.is, vote.nominee.is)
	}
      }
      
      votersList= Nil
      for (userId <- results.keySet) {
	// normalize voting weight
	val head= getUserHead(userId)
	val vec= results.get(userId).get
	vec.normalize()
	if (vec.distanceTo(head.vec) > eps) {
	  head.latestUpdate= time
	  votersList ::= userId
	}
	// make the updated weights visible
	head.vec= vec
      }
      // determine affected delegating users
      val dlgts= Votable.get(votersList.map{ users.get(_).get.user }, room)
      votersList= Vote.findAll(ByList(Vote.nominee, dlgts.map{ _.id.is })).map{ _.owner.is }
      iterCount+= 1
    }
  }
}

