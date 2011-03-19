package org.liquidizer.lib

import net.liftweb.mapper._
import org.liquidizer.model._

import VoteMap.latestUpdate
import VoteMap.users
import VoteMap.nominees
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
class UserHead(val user : User) {
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
object Solver {
  /** Recompute all results following the latest votes */
  def recompute() : Unit = {
    // iterative matrix solving
    sweep(1000, 1e-4)

    // Prepare result map
    var resultMap= Map(nominees.keys.toSeq.filter{_.isQuery}.map{ case VotableQuery(query) => query.id.is -> Quote(0,0)}:_*)
    // collect the results for each nominee
    for (user <- users) {
      var active= false
      val head= user._2
      head.vec.votes.elements.foreach { case (i, e) =>
	val w= e.value * head.weight(latestUpdate)
	if (w.abs > EPS) {
	  active= true
	  if (!resultMap.contains(i)) resultMap += i -> Quote(0,0)
	  resultMap.get(i).get += w
	}
      }
      // remember if this user actively participates
      head.active= active
    }
    // persist election results
    resultMap.foreach { case (i,quote) => 
      setResult(VotableQuery(Query.get(i).get), quote) }

    // normalize popularity to 1
    val denom= 
      1.0/Math.sqrt(resultMap.foldLeft(1e-8)
		    { (a,b) => a + Math.pow(b._2.volume,2.0) })
    // compute popularity as dot product with result vector
    for (user <- users) {
      val vec= user._2.vec
      var pop= Quote(0,0)
      if (user._2.active) {
        vec.votes.elements.foreach { case (i, e) =>
	  val w= e.value * user._2.weight(latestUpdate)
	  resultMap.get(i).foreach { q => pop = pop + q * w }
	}
      }
      setResult(VotableUser(user._1), pop*denom)
    }
  }

  def setResult(nominee : Votable, quote : Quote) = {
    nominees.get(nominee).getOrElse {
      val head= new NomineeHead(nominee)
      nominees+= (nominee -> head)
      head
    }.update(latestUpdate, quote)
  }

  /** Iterative solution of the users voting vectors */
  def sweep(maxIter : Int, eps : Double) : Unit = {
    // read votes
    var votes = Vote.findAll(By_>(Vote.date, latestUpdate))
    latestUpdate= votes.map { _.date.is }.foldLeft(0L) { _ max _ }

    var followCache= Map[User, List[User]]()
    var voteCache= Map[User, List[Vote]]()
    var list= votes.map {_.owner.obj.get}.removeDuplicates
    var iterCount= 0

    // update user info
    for (user <- list) {
      if (!users.contains(user))
       users += (user -> new UserHead(user))
      val uHead= users.get(user).get
      val uvotes= Vote.findAll(By(Vote.owner, user))

      // update the latest activity time
      uvotes.foreach { v => uHead.update(v.date.is) }
      
      // delete zero votes, keep one vote for update Time
      for (v <- uvotes) {
       if (v.weight.is==0 && v.date.is < uHead.latestVote) {
         v.delete_!
       }
      }
      voteCache += user -> uvotes.filter { _.weight.is != 0 }
    }

    // repeat until convergence is reached
    while (!list.isEmpty && iterCount<maxIter) {
      var nextList= List[User]()
      for (user <- list) {
	val head= users.get(user).get

	// reset the voting vector
	val decay= head.weight(latestUpdate)
	val vec= new VoteVector(user.id.is)
	
	// vor each vote cast by the user update the voting vector
	if (!voteCache.contains(user)) {
	  voteCache += user -> Vote.findAll(By(Vote.owner, user)).filter(_.weight!=0)
	}
	for (vote <- voteCache.get(user).get) {
	  vote.nominee.obj.get match {
            case VotableUser(user) => 
	    	// the vote is a delegation, mix in delegate's voting weights
		val uHead= users.get(user)
               if (!uHead.isEmpty)
                 vec.addDelegate(vote.weight.is, uHead.get.vec)
	    case VotableQuery(query) => 
	      // the vote is cast on a query
	      vec.addVote(vote.weight.is, query.id.is)
	    case _ =>
	  }
	}

	// ensure the global voting weight constraint
	vec.normalize()
	if (vec.distanceTo(head.vec) > eps) {
	  head.latestUpdate= latestUpdate
	  // process followers
	  if (!followCache.contains(user))
	    followCache += user -> VoteMap.getActiveVoters(VotableUser(user))
	  nextList ++= followCache.get(user).get
	}
	// make the updated weights visible
	head.vec= vec
	iterCount+= 1
      }
      list= nextList.removeDuplicates
    }
  }
}
