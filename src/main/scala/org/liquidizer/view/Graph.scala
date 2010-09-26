package org.liquidizer.view

import scala.collection.mutable
import net.liftweb.mapper._

import org.liquidizer.lib._
import org.liquidizer.model._

case class Edge(var from:User, 
                var to:Votable, 
                var weight:Double, 
                var dashed:Boolean)

class Graph {
  val nodes= mutable.Set.empty[Votable]
  val edges= mutable.Map.empty[(User,Votable),Edge]  

  def getEdges() : Iterable[Edge] = edges.values.toList
  def getNodes() : Iterable[Votable] = nodes
}

class DelegationGraph extends Graph{
  val blocked= mutable.Set.empty[Votable]
  
  def putEdge(user:User, nominee:Votable, weight:Double, dashed:Boolean):Unit = {
    edges.put((user,nominee), Edge(user, nominee, weight, dashed))	  
  }
  
  def putEdge(user:User, nominee:Votable, dashed:Boolean):Unit = {
    val weight= VoteCounter.getResult(user, nominee)
    putEdge(user, nominee, weight, dashed)
  }
  
  def buildFromQuery(nominee : Votable) : Unit = {
    if (!nodes.contains(nominee)) {
      nodes += nominee
      VoteCounter.getSupporters(nominee, false).foreach {
	user =>
	  putEdge(user,nominee,false)
	buildFromQuery(VotableUser(user))
      }
    }	  
  }
  
  def findIndirectRoute(start:User, nominee:Votable, weight:Double) {
    nominee match {
      case VotableUser(delegate) =>
	if (!blocked.contains(nominee)) {
	  blocked += nominee
	  VoteCounter.getVotes(delegate, false).foreach {
	    vote =>
	      if (!edges.contains(delegate,vote)) {
		val factor= VoteCounter.getResult(delegate, vote)
		findIndirectRoute(start, vote, weight * factor)
	      }
	  }
	}
      case VotableQuery(query) =>
	if (nodes.contains(nominee)) {
	  edges.get((start,nominee)) match {
	    case Some(edge @ Edge(_,_,_,true)) => edge.weight+=weight
	    case None => putEdge(start, nominee, weight, true)
	    case _ =>
	  }
	}
    }
  }
  
  def buildForUser(user : User) : Unit = {
    if (!blocked.contains(VotableUser(user))) {
      blocked+= VotableUser(user)
      VoteCounter.getVotes(user, false).foreach {
	vote => 
	  putEdge(user, vote, false)
	nodes += vote
	vote match {
	  case VotableUser(delegate) => buildForUser(delegate)
	  case _ =>
	}
      }
    }
  }
}
