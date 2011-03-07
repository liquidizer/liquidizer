package org.liquidizer.view

import scala.xml.NodeSeq
import java.util.{PriorityQueue, Comparator}

import net.liftweb.mapper._
import net.liftweb.common._

import org.liquidizer.lib._
import org.liquidizer.model._

case class Edge(val from: User, val to: Votable)
case class Entry(val node: Votable,  weight: Double)

class GraphvizAPI(root : Votable) extends CommandAPI("dot -Tsvg") {

  var nodes= Set[Votable]()
  var edges= Set[Edge]()
  val queue= new PriorityQueue[Entry] (10, new Comparator[Entry] {
    def compare(x: Entry, y: Entry) = -(x.weight.abs compare y.weight.abs)
  })

  def formatDouble(value : Double)= String.format("%3.2f",double2Double(value))

  def getWeight(node : Votable) = {
    def sqr(x:Double)= x*x
    root match {
      case VotableUser(user1) => node match {
	case VotableQuery(_) =>  sqr(VoteCounter.getWeight(user1, node))
	case VotableUser(user2) => VoteCounter.getSympathy(user1, user2)
      }
      case _ => node match {
	case VotableUser(user) => sqr(VoteCounter.getWeight(user, node))
	case _ => 0.0
      }
    }
  }

  def addEntry(node : Votable) = queue.add(Entry(node, getWeight(node)))

  def process(node : Votable) = {
    for (user <- VoteCounter.getActiveVoters(node)) {
      val edge= Edge(user, node)
      if (!edges.contains(edge)) {
	edges+=edge
      }
      if (!nodes.contains(VotableUser(user))) addEntry(VotableUser(user))
    }
    node match {
      case VotableUser(user) => 
	for (nominee <- VoteCounter.getActiveVotes(user)) {
	  val edge= Edge(user, nominee)
	  if (!edges.contains(edge)) {
	    edges+= edge
	  }
	  if (!nodes.contains(nominee)) addEntry(nominee)
	}
      case _ =>
    }
  }

  def build(nominee : Votable, size : Int) : Unit = {
    nodes+= nominee
    process(nominee)
    while (nodes.size < size && !queue.isEmpty) {
      val cur= queue.poll
      if (!nodes.contains(cur.node)) {
	nodes += cur.node
	process(cur.node)
      }
    }
  }
  
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
	out("\""+node.uri+"\" ["+opt+" label=\""+label+"\" shape=\"circle\" width=1]")
	case node @ VotableQuery(query) => 
	  no+= 1
	  out("\""+node.uri+"\" ["+opt+" label=\""+no+"\" shape=\"box\"]")
      }}
    edges
    .filter{ e=>nodes.contains(VotableUser(e.from)) && nodes.contains(e.to)}
    .foreach{ e=>
      var opt=""
      if (e.to.isQuery) {
	val w= VoteCounter.getWeight(e.from, e.to)
	opt= "[color=\""+(if (w>=0) "black" else "red")+"\"]"
      }
      out("\""+VotableUser(e.from).uri+
	  "\" -> \""+e.to.uri+"\" "+opt)
    }
    out("}")
    getSVG()
  }
}
