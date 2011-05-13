package org.liquidizer.snippet

import scala.xml._
import scala.collection.mutable

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._
import Helpers._

import _root_.org.liquidizer.lib._
import _root_.org.liquidizer.model._

class BPT extends MultipageSnippet {

  val isWiki= S.param("wiki").exists(_!="false")
  order= Some("bpt")
  
  def getData() = {
    if (data.isEmpty) loadData()
    data
  }
    
  def loadData() = {
    var queries = Query.findAll.filter { searchFilter _ }
    if (order.exists{_=="bpt"})
      queries= queries.filter ( !_.keys.is.contains("#vorselektiert") )
    data= Votable.findAll(ByList(Votable.query, queries.map {_.id.is}))
    sortData()
  }

  val BLOCK_R= "(#block[0-9]+)".r
  val URL_R= "https?:.*".r
  def render(in: NodeSeq) : NodeSeq = {
    var block : Option[String]= None
    var no= (0,0)

    val helper= new VotingHelper
    helper.no= from
    getData()
    .flatMap {
      item =>
	item match {
	  case VotableQuery(query) =>
	    val m= BLOCK_R.findFirstIn(query.keys.is)
	    if (m.isEmpty) no= (no._1+1, 0)
	      else {
		if (m==block) {
		  no= (no._1, no._2+1)
		} else {
		  no= (no._1+1, 1)
		}
	      }
	    block= m
	  case _ => 
	}
        val title= item.query.obj.get.what.is.replaceAll(" http:.*","")
        if (isWiki) {
	    val prefix= if (no._2 == 0) Text("#")
			else if (no._2 ==1) {<span>{Text("#")}</span> ++ <br/> ++Text("##")}
			else Text("##")
	    val url= URL_R.findFirstIn(item.query.obj.get.what.is)
	  prefix ++ 
	    Text({" ["+url.getOrElse("")+" "+title+"]" +
	    " ("+("%1.2f".format(VoteMap.getCurrentResult(item).value))+")"
	  }) ++ <br/>
	} else {
	  Helpers.bind("bpt", helper.bind(in, item),
		       "no" -> {if (no._2==0) Text(no._1.toString)
				else Text(no._1+"."+no._2)},
		       "title" -> <a>{title}</a>,
		       "sep" -> (if (no._1 >1 && no._2 <= 1) {
			 <tr><td colspan="5"><hr/></td></tr>
		       } else {
			 NodeSeq.Empty
		       })
		     )
	}
    }
  }
}
