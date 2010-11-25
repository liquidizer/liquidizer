package org.liquidizer.lib

class VoteVector(
  val userID : Int,
  var votes : Array[Double],
  var supporters : Array[Double]) {
  
  def this(userID : Int) = this(userID, new Array[Double](0), new Array[Double](userID+1))
  
  def normalize(vec : Array[Double], norm : Double) =
    for (i <- 0 to vec.length-1)
      vec(i)= vec(i)/norm;
  
  def normalize() : Unit = {
    val norm= votes.foldLeft(0.0) { (a,b) => a+b*b }
    if (norm > 1e-8) {
      normalize(votes, Math.sqrt(norm))
    }
  }
  
  def clear(vec : Array[Double], value : Double) = {
    for (i <- 0 to vec.length-1) vec(i) = value
  }
  
  /** clear all weights */
  def clear(weight: Double) : Unit = {
    clear(votes, 0.0)
    clear(supporters, 0.0)
    supporters(userID)= weight
  }

  def enlarge(vec : Array[Double], minLength : Int) = {
    if (vec.length >= minLength)
      vec
    else
      vec ++ (new Array[Double](minLength - vec.length))
  }

  def add(weight : Double, vec1 : Array[Double], vec2 : Array[Double]) = {
    for (i <- 0 to vec2.length-1)
      vec1(i) += vec2(i)
  }

  def addSupporter(weight : Double, other : VoteVector) = {
    val len= supporters.length
    val olen= other.supporters.length
    val sup= other.getInflow
    supporters= enlarge(supporters, olen)
    // add the delegation weights for all users
    val circular= if (olen>userID) other.supporters(userID) else 0.0
    for (i <- 0 to olen-1)
      supporters(i) += weight * (other.supporters(i) - circular*supporters(i)).max(0)
  }
  
  def addVote(weight : Double, queryId : Int) = {
    votes= enlarge(votes, queryId+1)
    votes(queryId) += weight
  }

  def addDelegate(weight : Double, delegate : VoteVector) = {
    votes= enlarge(votes, delegate.votes.length);
    for (i <- 0 to delegate.votes.length-1)
      votes(i)+= weight * delegate.votes(i)
  }

  def dotProd(other : VoteVector, abs: Boolean) : Double = {
    votes= enlarge(votes, other.votes.length)
    var scalar= 0.0
    for (i <- 0 to other.votes.length-1)
      if (abs)
	scalar += Math.abs(votes(i)*other.votes(i))
      else
	scalar += votes(i)*other.votes(i)
    scalar * getActiveWeight * other.getActiveWeight
  }
  
  def getVotingWeight(queryId : Int) : Double = 
    if (queryId < votes.length) getActiveWeight * votes(queryId) else 0.0
  
  def getActiveWeight() = supporters(userID)

  def getSupportWeight(userId : Int) =
    if (userId < supporters.length) supporters(userId) else 0.0

  def getInflow() : Double = supporters.reduceLeft { _+_ }

  override def toString() = userID+" : "+
    votes.map { x=> Math.round(x*100.0)/100.0 }.mkString("[",", ","]") + " : "+
    supporters.map { x=> Math.round(x*100.0)/100.0 }.mkString("[",", ","]")
}

