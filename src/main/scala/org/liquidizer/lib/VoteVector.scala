package org.liquidizer.lib

class VoteVector(val userID : Int, 
		 var votes : Array[Double], 
		 var idols : Array[Double]) {

  def this(userID : Int) = this(userID, new Array[Double](0), new Array[Double](userID+1))

  def this(other : VoteVector) = 
    this(other.userID, other.votes.clone, other.idols.clone)

  def normalize(vec : Array[Double], norm : Double) =
    for (i <- 0 to vec.length-1)
      vec(i)= vec(i)/norm;
  
  def normalize() : Unit = {
    val norm= votes.foldLeft(0.0) { (a,b) => a+b*b }
    if (norm > 1e-8)
      normalize(votes, Math.sqrt(norm))
    val maxD= Math.max(
      idols.slice(0, userID).foldLeft(0.) { _ max _},
      idols.drop(userID+1).foldLeft(0.) { _ max _})
    if (maxD > 1e-8)
      normalize(idols, maxD)
  }
  
  def clear(vec : Array[Double], value : Double) = {
    for (i <- 0 to vec.length-1) vec(i) = value
  }
  
  /** clear all weights */
  def clear() : Unit = {
    clear(votes, 0.0)
    clear(idols, 0.0)
    idols(userID)= 1.0
  }

  /** enlarge a vector if necessary */
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

  def addVote(weight : Double, queryId : Int) = {
    votes= enlarge(votes, queryId+1)
    votes(queryId) += weight
  }

  def addDelegate(weight : Double, other : VoteVector) = {
    votes= enlarge(votes, other.votes.length);
    for (i <- 0 to other.votes.length-1)
      votes(i)+= weight * other.votes(i)

    val len= idols.length
    val olen= other.idols.length
    idols= enlarge(idols, olen)
    // add the delegation weights for all users
    val circular= if (olen>userID) other.idols(userID) else 0.0
    for (i <- 0 to olen-1)
      idols(i) += weight * (other.idols(i) - circular*idols(i)).max(0)

  }

  def dotProd(other : VoteVector, abs: Boolean) : Double = {
    votes= enlarge(votes, other.votes.length)
    var scalar= 0.0
    for (i <- 0 to other.votes.length-1)
      if (abs)
	scalar += Math.abs(votes(i)*other.votes(i))
      else
	scalar += votes(i)*other.votes(i)
    scalar
  }
  
  def distanceTo(other : VoteVector) : Double = {
    var dist= 0.0
    for (i <- 0 to Math.min(votes.length, other.votes.length)-1)
      dist = dist.max((votes(i)-other.votes(i)).abs)
    for (i <- 0 to Math.min(idols.length, other.idols.length)-1)
      dist = dist.max((idols(i)-other.idols(i)).abs)
    dist
  }

  def getVotingWeight(queryId : Int) : Double = 
    if (queryId < votes.length) votes(queryId) else 0.0
  
  def getDelegationWeight(idolId : Int) =
    if (idolId < idols.length) idols(idolId) else 0.0

  def getMaxDelegation() : Double = idols.reduceLeft { _ max _ }

  override def toString() = userID+" : "+
    votes.map { "#.##".format(_) }.mkString("[",", ","]") + " : "+
    idols.map { "#.##".format(_) }.mkString("[",", ","]")
}

