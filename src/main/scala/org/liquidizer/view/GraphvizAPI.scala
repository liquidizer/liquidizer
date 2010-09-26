package org.liquidizer.view

import scala.collection.mutable
import scala.xml.NodeSeq
import java .io._
import net.liftweb.mapper._

import org.liquidizer.lib._
import org.liquidizer.model._

class GraphvizAPI {
  val p = Runtime.getRuntime().exec("dot -Tsvg");  
  val stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
  val stdOutput = new BufferedWriter(new OutputStreamWriter(p.getOutputStream()));
  val stdError = new BufferedReader(new InputStreamReader(p.getErrorStream()));

  def out(line:String) = { stdOutput.write(line+"\n") }
  
  def toGraph() : NodeSeq = {
    val graph= new DelegationGraph
    Query.findAll.foreach { query => graph.buildFromQuery(VotableQuery(query)) }
    runGraphviz(graph, None)
  }
  
  def toGraph(nominee : Votable) : NodeSeq = {
    val graph= new DelegationGraph
    graph.buildFromQuery(nominee)
    runGraphviz(graph, Some(nominee))
  }
  
  def toGraph(user : User) : NodeSeq = {
    val graph= new DelegationGraph
    graph.buildFromQuery(VotableUser(user))
    graph.buildForUser(user)
    graph.getNodes().foreach { node => node match { 
      case VotableUser(user) => graph.findIndirectRoute(user, node, 1.0)
      case _ =>
    }}
    runGraphviz(graph,Some(VotableUser(user)))
  }
  
  private def runGraphviz(graph : Graph,
                          head : Option[Votable]):NodeSeq = {
    out("digraph delegationMap {")
    out("size=\"12,12\"")
    graph.getNodes().foreach {
      node =>
	var options= ""
      if (Some(node)==head)
	options= options+"color=\"coral\" style=\"filled\""
      node match {
	case node @ VotableUser(user) => 
	  val label= user.toString.replaceAll(" ","\\\\n")
	out("\""+node.uri+"\" ["+options+" label=\""+label+"\" shape=\"circle\" width=1]")
	case node @ VotableQuery(query) => 
	  out("\""+node.uri+"\" ["+options+" label=\""+node.id+"\" shape=\"box\"]")
      }}
    graph.getEdges.foreach {
      case Edge(from,to,result,dashed) =>
	val label= String.format("%1.2f",double2Double(result)).replaceAll("0\\.","\\.")
      var options= "color=\"" + {
	if (dashed) {
	  if (result>=0) "#c7fbb3" else "#ffc8a2"
	} else {
	  if (result>=0) "darkgreen" else "red"
	} } +"\""
      if (dashed) {
	options += " weight=0"
      } else {
	options += " label=\""+label+"\""
      }
      out("\""+VotableUser(from).uri+
	  "\" -> \""+to.uri+
	  "\" ["+options+"]")
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
