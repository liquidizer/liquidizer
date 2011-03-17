package org.liquidizer.lib

class SparseVec {
  class Entry(var value : Double)
  var map= Map[Long, Entry]()
  
  private def entry(index : Long) =
    map.get(index).getOrElse {
      val e= new Entry(0.0)
      map += index -> e
      e
    }

  def get(index : Long) = map.get(index).map { _.value }.getOrElse(0.0)
  def set(index : Long, value : Double) = { entry(index).value= value }

  def add(weight : Double, other : SparseVec) : Unit =
    other.map.foreach { 
      case (i, e) => 
	if (e.value.abs > 1e-8) entry(i).value += weight * e.value 
    }

  def dotprod(other : SparseVec, f : Double => Double) : Double =
    other.map.foldLeft(0.) { (a,b) => a + f( b match {
      case (i, e) => map.get(i).map { e.value * _.value }.getOrElse(0.0)
    })}

  def norm() : Double= 
    Math.sqrt(map.toSeq.foldLeft(0.) { case (n,(_, e)) => n + e.value*e.value })

  def map(f : Double => Double) : Unit =
    map.foreach { case (_,v) => { v.value = f(v.value) }}

  def distanceTo(other : SparseVec) : Double = {
    var dist=0.0
    map.foreach { case(i,e) => 
      dist = dist max (e.value - other.get(i)).abs
	       }
    other.map.foreach { case(i,e) => 
      if (!map.contains(i))
	dist = dist max e.value.abs
		     }
    dist
  }
  override def toString() = map.toString
}

class VoteVector(val userID : Long) {
  val votes = new SparseVec
  val idols = new SparseVec
  var maxD = 0.0

  idols.set(userID, 1.0)

  def normalize() : Unit = {
    val norm= votes.norm
    if (norm > 1e-8)
      votes.map { x => x/norm }
    idols.map.foreach { 
      case (i, e) => if (i!=userID) maxD = maxD max e.value
    }
  }
  
  def addVote(weight : Double, queryId : Long) = {
    votes.set(queryId, votes.get(queryId)+weight)
  }

  def addDelegate(weight : Double, old : VoteVector, other : VoteVector) = {
    votes.add(weight, other.votes)
    idols.add(weight, other.idols)
    val circular= other.idols.get(userID)
    if (circular>1e-8) idols.add(-weight*circular, old.idols)
    idols.map { _ max 0.0 }
  }

  def dotProd(other : VoteVector, abs: Boolean) : Double = {
    if (abs)
      votes.dotprod(other.votes, x => x.abs)
    else
      votes.dotprod(other.votes, x => x)
  }
  
  def distanceTo(other : VoteVector) : Double = {
    votes.distanceTo(other.votes) + 0.01*idols.distanceTo(other.votes)
  }

  def getVotingWeight(queryId : Long) = votes.get(queryId)
  def getDelegationWeight(idolId : Long) = idols.get(idolId)/(maxD max 1e-8)

  override def toString() = userID+" : "+votes+" : "+idols
}

