package org.liquidizer.view

import scala.xml.NodeSeq
import java.util.{PriorityQueue, Comparator}

import net.liftweb.mapper._
import net.liftweb.common._

import org.liquidizer.lib._
import org.liquidizer.model._

case class Edge(val from: User, val to: Votable)

/** Control graphviz to plot a dependency graph centered around a root node */
class GraphvizAPI(root : Votable) extends CommandAPI("dot -Tsvg") {

  var nodes= List[Votable]()
  var edges= Set[Edge]()
  var weight= Map[Votable, Double]()
  val queue= new PriorityQueue[Votable] (10, new Comparator[Votable] {
    def compare(x: Votable, y: Votable) = -(weight(x).abs compare weight(y).abs)
  })

  /** Compute the priority with which a node should be added to the graph */
  def computeWeight(node : Votable) = {
    if (!weight.contains(node)) {
      def sqr(x:Double)= x*x
      weight += node -> (root match {
	case VotableUser(user1) => node match {
	  case VotableQuery(_) =>  sqr(VoteMap.getWeight(user1, node))
	  case VotableUser(user2) => 
	    VoteMap.getWeight(user1, node) +
	    VoteMap.getWeight(user2, root)
	}
	case _ => node match {
	  case VotableUser(user) => sqr(VoteMap.getWeight(user, node))
	  case _ => 0.0
	}
      })
    }
  }

  /** add an entry to the queue of connected nodes */
  def addEntry(node : Votable) = {
    computeWeight(node)
    queue.add(node)
  }

  /** continue searching for connected nodes */
  def process(node : Votable) = {
    for (user <- VoteMap.getActiveVoters(node)) {
      val edge= Edge(user, node)
      if (!edges.contains(edge)) {
	edges+=edge
      }
      if (!nodes.contains(VotableUser(user))) addEntry(VotableUser(user))
    }
    node match {
      case VotableUser(user) => 
	for (nominee <- VoteMap.getActiveVotes(user, node.room.obj.get)) {
	  val edge= Edge(user, nominee)
	  if (!edges.contains(edge)) {
	    edges+= edge
	  }
	  if (!nodes.contains(nominee)) addEntry(nominee)
	}
      case _ =>
    }
  }

  /** build the graph centered around nominee with a maximum number of nodes */
  def build(nominee : Votable, size : Int) : Unit = {
    nodes::= nominee
    process(nominee)
    while (nodes.size < size && !queue.isEmpty) {
      val cur= queue.poll
      if (!nodes.contains(cur) && weight(cur)!=0) {
	nodes::= cur
	process(cur)
      }
    }
    nodes= nodes.reverse
  }
  
  /** Write dot code and run graphviz */
  def runGraphviz() : NodeSeq = {
    out("digraph delegationMap {")
    out("size=\"12,12\"")
    var no=0
    nodes.foreach {
      node =>
      var opt= ""
      if (node==root)
	opt= "style=\"filled\" fillcolor=\"#660099\" fontcolor=\"white\""
      node match {
	case node @ VotableUser(user) => 
	  val label= user.toString.replaceAll(" ","\\\\n")
	out("\""+node.uri+"\" ["+opt+" label=\""+label+"\" shape=\"circle\"]")
	case node @ VotableQuery(query) => 
	  no+= 1
	  out("\""+node.uri+"\" ["+opt+" label=\""+no+"\" shape=\"box\"]")
      }}
    edges
    .filter{ e => nodes.contains(VotableUser(e.from)) && nodes.contains(e.to)}
    .foreach{ e=>
      var opt=""
      if (e.to.isQuery) {
	val w= VoteMap.getWeight(e.from, e.to)
	opt= "[color=\""+(if (w>=0) "black" else "red")+"\"]"
      }
      out("\""+VotableUser(e.from).uri+
	  "\" -> \""+e.to.uri+"\" "+opt)
    }
    out("}")
    getSVG()
  }
}
