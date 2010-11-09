package org.liquidizer.view

import scala.xml.NodeSeq
import scala.collection.mutable
import java.io._
import java.util._

import net.liftweb.mapper._

import org.liquidizer.lib._
import org.liquidizer.model._

case class Edge(val from: User, val to: Votable)
case class Entry(val node: Votable,  edge : Edge, weight: Double)

class GraphvizAPI {
  val p = Runtime.getRuntime().exec("dot -Tsvg");  
  val stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
  val stdOutput = new BufferedWriter(new OutputStreamWriter(p.getOutputStream()));
  val stdError = new BufferedReader(new InputStreamReader(p.getErrorStream()));

  val nodes= mutable.Set.empty[Votable]
  val edges= mutable.Set.empty[Edge]
  val queue= new PriorityQueue[Entry] (10, new Comparator[Entry] {
    def compare(x: Entry, y: Entry) = -(x.weight.abs compare y.weight.abs)
  })
  val options= mutable.Map.empty[Any, String]

  def formatDouble(value : Double)= String.format("%3.2f",double2Double(value))

  def setOptions(edge : Edge) : Double = {
    var label= 1.0
    var factor= 1.0
    edge.to match {
      case VotableQuery(_) => 
	label= VoteCounter.getWeight(edge.from, edge.to)
        factor= label * 0.1
      case VotableUser(_) => 
	factor= VoteCounter.getCumulativeWeight(edge.from, edge.to)
        label= factor * VoteCounter.getDelegationInflow(edge.from)
    }
    options.put(edge, 
		"color=\""+(if (factor>=0) "black" else "red")+"\" " +
		"label=\""+formatDouble(label)+"\"")
    factor
  }

  val yellow= SVGUtil.parseColor("#f4e514")
  val red= SVGUtil.parseColor("#CC0000")
  val white= SVGUtil.parseColor("#FFFFFF")

  def blend(color1: (Int,Int,Int), color2: (Int,Int,Int), w:Double) =
    ((color1._1 * w + color2._1 * (1-w)).toInt,
     (color1._2 * w + color2._2 * (1-w)).toInt,
     (color1._3 * w + color2._3 * (1-w)).toInt)

  def setOptions(node : Votable, weight : Double) = {
    val color=
      if (weight>0)
	blend(yellow, white, weight min 1.0)
      else
      	blend(red, white, (-weight) min 1.0)
    options.put(node, 
		"style=\"filled\"" +
		"color=\""+SVGUtil.formatColor(color._1, color._2, color._3)+"\"")
  }

  def process(node : Votable, weight : Double) = {
    for (user <- VoteCounter.getActiveVoters(node)) {
      val edge= Edge(user, node)
      if (!options.contains(edge)) {
	val factor= setOptions(edge)
	queue.add(Entry(VotableUser(user), edge, weight * factor))
      }
    }
    node match {
      case VotableUser(user) => 
	for (nominee <- VoteCounter.getActiveVotes(user)) {
	  val edge= Edge(user, nominee)
	  if (!options.contains(edge)) {
	    val factor= setOptions(edge)
	    queue.add(Entry(nominee, edge, weight * factor * 0.1))
	  }
	}
      case _ =>
    }
  }

  def toGraph(nominee : Votable, size : Int) : NodeSeq = {
    nodes+= nominee
    options.put(nominee,"style=\"filled\" fillcolor=\"#662e91\" fontcolor=\"white\"")
    process(nominee, 1.0)
    while (nodes.size < size && !queue.isEmpty) {
      val cur= queue.poll
      if (!edges.contains(cur.edge))
	edges+=cur.edge
      if (!nodes.contains(cur.node)) {
	nodes+= cur.node
	process(cur.node, cur.weight)
      }
    }
    runGraphviz()
  }
  
  def out(line:String) = { 
    stdOutput.write(line+"\n") 
  }
  
  def runGraphviz() : NodeSeq = {
    out("digraph delegationMap {")
    out("size=\"12,12\"")
    nodes.foreach {
      node =>
      var opt= options.get(node).getOrElse("")
      node match {
	case node @ VotableUser(user) => 
	  val label= user.toString.replaceAll(" ","\\\\n")
	out("\""+node.uri+"\" ["+opt+" label=\""+label+"\" shape=\"circle\" width=1]")
	case node @ VotableQuery(query) => 
	  out("\""+node.uri+"\" ["+opt+" label=\""+node.id+"\" shape=\"box\"]")
      }}
    edges.foreach {
      case edge @ Edge(from,to) =>
      out("\""+VotableUser(from).uri+
	  "\" -> \""+to.uri+"\" "+
	  options.get(edge).map { "["+_+"]" }.getOrElse(""))
    }
    out("}")
    getSVG()
  }
  
  def getSVG() : NodeSeq = {
    stdOutput.flush()
    stdOutput.close()

    val buf= new java.lang.StringBuilder
    var aws = stdInput.readLine
    while(aws!=null) {
      buf.append(aws+"\n")
      aws= stdInput.readLine
    }

    val src=scala.io.Source.fromString(buf.toString)
    val doc=scala.xml.parsing.XhtmlParser.apply(src)
    doc
  }

}
