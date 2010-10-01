package org.liquidizer.lib

class VoteVector(
  val userID : Int,
  var votes : Array[Double], 
  var delegates : Array[Double]) {
  
	def this(userID : Int) = this(userID, new Array[Double](0), new Array[Double](userID+1))
  
	def normalize(vec : Array[Double], norm : Double) =
		for (i <- 0 to vec.length-1)
			vec(i)= vec(i)/norm;
  
	def normalize() : Unit = {
		val norm= votes.foldLeft(0.0) { (a,b) => a+b*b }
		if (norm > 1e-8) {
			normalize(votes, Math.sqrt(norm))
		}
		val total= delegates.foldLeft(0.0) { (a,b) => a + Math.abs(b) }
		if (total > 1e-6)
			normalize(delegates, total)
	}
 
	def clear(vec : Array[Double]) = {
		for (i <- 0 to vec.length-1) vec(i) = 0.0
	}
 
	def clear() : Unit = {
		clear(votes)
		clear(delegates)
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
 
	def addDelegate(weight : Double, delegate : VoteVector) = {
		votes= enlarge(votes, delegate.votes.length);
		delegates= enlarge(delegates, delegate.delegates.length);

		for (i <- 0 to delegate.votes.length-1)
			votes(i)+= weight * delegate.votes(i)
		for (i <- 0 to delegate.delegates.length-1)
			delegates(i)+= weight * delegate.delegates(i)
	}

	def addVote(weight : Double, queryId : Int) = {
		votes= enlarge(votes, queryId+1)
		votes(queryId) += weight
		delegates(userID) += Math.abs(weight)
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
 
	def getVotingWeight(queryId : Int) = 
		if (queryId<votes.length) votes(queryId) else 0.0
	
	def getDelegationWeight(userId : Int) = 
		if (userId<delegates.length) delegates(userId) else 0.0
 
	override def toString() = userID+" : "+votes.mkString("[",",","]") + " : "+delegates.mkString("[",",","]")
}
