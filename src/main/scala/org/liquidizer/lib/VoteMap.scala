package org.liquidizer.lib

import net.liftweb.mapper._
import net.liftweb.common.Logger
import org.liquidizer.model._

/** The VoteCounter keeps track of all cast votes.
 *  It provides fast access to the latest results and statistical data.
 */
object VoteMap {
  val SWING_DECAY= 0.05 / Tick.day
  val WEIGHT_DECAY= 0.01 / Tick.day
  val EPS= 1e-5
  var latestUpdate = 0L

  private var solverMap = Map[Long, Solver]()
  private def solver(room : Long) : Solver = solverMap.get(room).getOrElse {
    solverMap+= room -> new Solver(Room.get(room).get)
    solverMap.get(room).get
  }

  /** Update to the current time */
  def refresh(blocking : Boolean = true) = update(Tick.now, blocking)
  
  /** Update to at least the given time in a thread save manner. */
  def update(time : Long, blocking : Boolean) = synchronized {
    if (latestUpdate<=time) {
      // run the recomputation with lower thread priority
      val thread= new Thread() with Logger {
	override def run() = {
	  val t0= Tick.now
	  updateSolver()
	  val t1= Tick.now
	  info("Vote results update took "+(t1-t0)+" ms")
	}}
      thread.setPriority(Thread.MIN_PRIORITY)
      thread.start
      if (blocking) thread.join
    }
  }

  private def updateSolver() : Unit = {
    // read votes
    val votes = Vote.findAll(By_>(Vote.date, latestUpdate))
    for (vote <- votes)
      solver(vote.nominee.obj.get.room.is).voteList::= vote

    // recompute
    val time= votes.map { _.date.is }.foldLeft(0L) { _ max _ }
    for (s <- solverMap.values) { 
      s.recompute() 
    }
    latestUpdate=  time
  }

  /** Get a measure of how much the vote changed recently */
  def getSwing(nominee : Votable) : Double = {
    solver(nominee.room.is).nominees.get(nominee.id.is).map { 
      head => (head.result.value - head.smooth) }.getOrElse(0.0)
  }

  /** get the global voting result for the nominee */
  def getCurrentResult(nominee : Votable) : Quote = {
    solver(nominee.room.is).nominees.get(nominee.id.is).map { 
      _.result(Tick.now) }.getOrElse(Quote(0,0))
  }

  /** get the active weight of the user */
  def getCurrentWeight(user : User, room : Room) : Double =
    solver(room.id.is).users.get(user.id.is).map { 
      _.weight(Tick.now) }.getOrElse(0.0)

  /** get the user's contribution to a vote on the nominee */
  def getWeight(user : User, nominee: Votable) : Double = {
    solver(nominee.room.is).users.get(user.id.is).map { uHead =>
      uHead.weight(Tick.now) * {
	if (nominee.isQuery) uHead.vec.getVotingWeight(nominee.query.is) 
	else uHead.vec.getDelegationWeight(nominee.user.is)
      }}.getOrElse(0.0)
  }

  /** Get the integer preference number */
  def getPreference(user : User, nominee : Votable) : Int = 
    Vote.get(user,nominee).map { _.weight.is }.getOrElse(0)

  /** Get the highest preference for a delegate */
  def getMaxDelegationPref(user : User, room : Room) = 
    solver(room.id.is).users.get(user.id.is).map{ 
      _.vec.maxIdolPref }.getOrElse(0)

  /** Check if the user is actively participating */
  def isActive(user : User, room : Room) : Boolean =
    user.validated && 
     solver(room.id.is).users.get(user.id.is).exists { _.active }

  def getAllVoters(nominee : Votable) : List[User] = {
    var list= Comment.findAll(By(Comment.nominee, nominee)).map { _.author.obj.get }
    // find voters recursively as voters and followers
    var proc= List(nominee)
    val room= nominee.room.obj.get
    while (!proc.isEmpty) {
      val votes= proc.flatMap{ n => Vote.findAll(By(Vote.nominee, n)) }
      val next= votes
      .filter{ _.weight.is!=0 }.map{ _.owner.obj.get }
      .removeDuplicates -- list
      proc= Votable.get(next, room)
      list ++= next
    }
    list
  }

  /** Find all queries a user is actively or indirectly voting for */
  def getAllVotes(user : User, room : Room) : List[Votable] = {
    // extract list from voting vector
    var list= List[Votable]()
    solver(room.id.is).users.get(user.id.is).foreach { uHead =>
      uHead.vec.votes.elements.foreach { case (i,e) =>
	if (e.value.abs > EPS) Votable.getQuery(i,room).foreach { list ::= _ }
      }
    }
    // include commentors
    val commentors= Comment.findAll(By(Comment.author, user))
    .map { _.nominee.obj.get }
    .filter { _.room.is == room.id.is }
    .filter { _.isQuery }
    // remove duplicates
    (list -- commentors) ++ commentors
  }

  /** Find voters that are actively participating with non zero preference */
  def getActiveVoters(nominee : Votable) : List[User] = {
    Vote.findAll(By(Vote.nominee, nominee))
    .filter { _.weight!=0 }
    .map { _.owner.obj.get }
  }

  /** Find votes a user is actively participating with non zero preference */
  def getActiveVotes(user : User, room : Room) : List[Votable] = {
    Vote.findAll(By(Vote.owner, user))
    .filter { _.weight!=0 }
    .map { _.nominee.obj.get }
    .filter { room.id.is == _.room.is }
  }

  /** Get currently weighted sympathy */
  def getSympathy(user1 : User, user2 : User, room : Room) : Double =
    getCurrentWeight(user1, room) *
    getCurrentWeight(user2, room) *
    getEmotion(user1, user2, room).map{ _.valence.is }.getOrElse(0.0)

  /** Get currently weighted arousal */
  def getArousal(user1 : User, user2 : User, room : Room) : Double =
    getCurrentWeight(user1, room) *
    getCurrentWeight(user2, room) *
    getEmotion(user1, user2, room).map{ _.arousal.is }.getOrElse(0.0)

  /** Get and update the emotion between two users */
  def getEmotion(user1 : User, user2 : User, room : Room) : Option[Emotion] = {
    if (isActive(user1, room) && isActive(user2, room)) {
      val emo= Emotion.get(user1, user2, room)

      // check if emotion needs to be updated
      val head1= solver(room.id.is).users.get(user1.id.is).get
      val head2= solver(room.id.is).users.get(user2.id.is).get

      // compute new emotion
      if (Math.max(head1.latestUpdate,head2.latestUpdate)>emo.time.is) {
	emo.update(latestUpdate, SWING_DECAY)
	emo.valence(head1.vec.dotProd(head2.vec, false))
	emo.potency(head1.vec.dotProd(head2.vec, true))
	emo.update(latestUpdate, SWING_DECAY)
	emo.save
      } else {
	// decay of emotional arousal
	emo.update(latestUpdate, SWING_DECAY)
      }      
      Some(emo)
    } else
      None
  }
}
