package org.liquidizer.lib

/** Efficient data structure for blocked sparse vectors */
class SparseVec {
  val buf= 8
  var data= Map[Long, Array[Double]]()
  case class Entry(val index : Int, val array : Array[Double]) {
    implicit def value() : Double = array(index)
    def set(value : Double) = { array(index)= value }
    def add(value : Double) = { array(index)+= value }
  }
  
  private def entry(index : Long, create : Boolean = true) =
    Entry((index % buf).toInt, data.get(index/buf).getOrElse {
      val e= new Array[Double](buf)
      if (create) data += (index/buf) -> e
      e
    })

  def get(index : Long) = entry(index, false).value
  def set(index : Long, value : Double) = { entry(index, true).set(value) }

  def elements() : Seq[(Long, Entry)] = 
    data.toSeq.flatMap { case (i,a) => 
      (0 to buf-1).map { j => (i*buf+j, Entry(j, a)) }}

  def add(weight : Double, other : SparseVec) : Unit =
    other.elements.foreach { case (i, e) => entry(i).add(weight*e.value) }

  def dotprod(other : SparseVec, f : Double => Double) : Double =
    other.elements.foldLeft (0.) { case (sum, (i,e)) => sum + f(e.value*get(i))}

  def norm() : Double= 
    Math.sqrt(elements.foldLeft (0.) { case (n, (_,e)) => n + e.value*e.value })

  def distanceTo(other : SparseVec) : Double = {
    var dist=0.0
    elements.foreach{ case (i,e) => dist = dist max (e.value-other.get(i)).abs }
    other.elements.foreach{ case (i,e) => dist = dist max (e.value-get(i)).abs }
    dist
  }

  override def toString() = 
    data.keySet.toList.sort{_ < _}
    .map{ i => "%d -> %2.2f".format(i,get(i)) }.mkString("(",",",")")
}

/** This class keeps track of voting weights and delegation influences */
class VoteVector(val userID : Long) {
  val votes = new SparseVec
  val idols = new SparseVec
  var maxD = 0.0
  var maxIdolPref = 0
  var denom = 0

  def normalize() : Unit = {
    val norm= votes.norm
    if (norm > 1e-8) {
      votes.elements.foreach { case (i,e) => e.set(e.value/norm) }
      idols.elements.foreach { case (i,e) => e.set(0.0 max e.value/denom) }
    }
    idols.elements.foreach { case (i,e) => maxD = maxD max e.value }
    idols.set(userID, 1.0)
  }
  
  def addVote(weight : Int, queryId : Long) = {
    denom+= weight.abs
    votes.set(queryId, votes.get(queryId)+weight)
  }

  def addDelegate(weight : Int, other : VoteVector) = {
    denom+= weight.abs
    maxIdolPref = maxIdolPref max weight
    votes.add(weight, other.votes)
    idols.add(weight, other.idols)
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

