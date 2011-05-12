package org.liquidizer.lib.bpt

import java.io._
import java.text._

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.model._
import org.liquidizer.lib._

object Stats {

  def orderDependency() {
    val queries= Query.findAll().sort { _.what.is < _.what.is }
    val stream= new FileOutputStream("position.csv");
    val out = new PrintStream(stream);

    for (query <- queries) {
      val name= query.what.is
      out.print(name.substring(0, name.indexOf(":")))
      out.print(",")
      val result= VoteMap.getCurrentResult(query.nominee)
      out.print(result.pro)
      out.print(",")
      out.print(result.contra)
      out.print(",")
      out.print(VoteMap.getActiveVoters(query.nominee).size)
      out.println()
    }
    out.close
  }

  def participation() {
    val stream= new FileOutputStream("participation.txt");
    val out = new PrintStream(stream);

    val voters= Vote.findAll
    .filter { _.weight.is!=0 }
    .map { _.owner.is }
    .removeDuplicates.size
    out.println("Voters : "+voters)
    
    val comments= Comment.findAll
    val no= comments.map{ _.author.is }.removeDuplicates.size
    out.println("Comments : "+comments.size)
    out.println("Authors : "+no)

    val buf= new StringBuffer
    comments.map { _.content.is }.foreach { 
      text =>
	buf.append(text)
        buf.append(" ")
    }
    
    val words= buf.toString.split(" ").size
    out.println("Words : "+words)

    out.close
  }

  def delegations() {
    var d1=0.0
    var d2=0.0
    val users= User.findAll.filter { VoteMap.isActive(_) }
    for (u1 <- users) {
      for (u2 <- users) {
	if (u1!=u2) {
	  if (VoteMap.getWeight(u1, VotableUser(u2))>0) {
	    d1 += 1.0
	    if (VoteMap.getWeight(u2, VotableUser(u1))>0) {
	      d2 += 1.0
	    }
	  }
	}
      }
    }
    val stream= new FileOutputStream("delegations.txt")
    val out = new PrintStream(stream);
    val n= users.size
    out.println("#Active users : "+ n)
    out.println("#A delegates B : "+(d1/n))
    out.println("#A and B delegate : "+(d2/n))

    out.println("digraph delegationMap {")
    out.println(" node[style=\"filled\" fillcolor=\"#660099\" width=0.5 height=0.5]")
    for (u <- users) {
      out.println(u.id.is+" [label=\"\"]")
    }
    for (u1 <- users) {
      for (u2 <- users) {
	if (u1.id.is>u2.id.is) {
	  val vote= Vote.find(By(Vote.owner, u1), By(Vote.nominee, VotableUser(u2)))
	  if (!vote.isEmpty) {
	    out.println(u1.id.is+" -> "+u2.id.is)
	  }
	}
      }
    }
    out.println("}")
    out.close()
    
  }
}
