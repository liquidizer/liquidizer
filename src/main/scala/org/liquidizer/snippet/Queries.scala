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

class Queries extends MultipageSnippet {
  
  def getData() = {
    if (data.isEmpty) loadData()
    data
  }
    
  def loadData() = {
    data = Query.findAll
    .filter { searchFilter _ }
    .map { VotableQuery(_) }
    sortData()
  }

  def sortData(): Unit = {
    sortData { q => Math.abs(VoteCounter.getResult(q).value) }
  }

  override def categories(in:NodeSeq) : NodeSeq = {
    val keys= search.split(" +").toList.distinct.map { _ toLowerCase }
    val markup= new CategoryView(keys, "/queries.html")
    <span>{
      markup.renderTagList(TaggedUtils.sortedTags(getData()), 5)
    }</span>
  }

  def render(in: NodeSeq) : NodeSeq = {
    val helper= new VotingHelper
    getData()
    .slice(from, to)
    .flatMap {
      item =>  helper.render(in, item)
    }
  }
}

class QueryDetails extends MultipageSnippet {
  val query= Query.getQuery(S.param("query").openOr("-1"))
  var hasMe= false;

  def loadData() = {
    println("load supporters")
    data= 
      VoteCounter.getAllVoters(query.get)
      .filter { searchFilter _ }
      .map { VotableUser(_) }
    
    sortData( _ match { 
      case VotableUser(user) => 
	VoteCounter.getDelegationInflow(user) *
	VoteCounter.getWeight(user, VotableQuery(query.get)).abs + (
	  if (VoteCounter.getComment(user, VotableQuery(query.get)).isEmpty)
	    0 else 1000)
    })

    // check if my own vote is registered. 
    // If not a page update should show it after the first vote is cast
    val me= User.currentUser
    hasMe= !me.isEmpty && data.contains(VotableUser(me.get))
  }

  val helper= new VotingHelper {
    override def getSupporters() : List[User] = {
      if (data.isEmpty)
	loadData()
      data.slice(from,to).map { case VotableUser(user) => user }
    }
    override def ajaxUpdate(votedNominee : Votable) : JsCmd = {
      val update= super.ajaxUpdate(votedNominee)
      if (!hasMe && votedNominee==VotableQuery(query.get)) {
	hasMe= true
	update & RedirectTo(VotableQuery(query.get).uri+"/index.html")
      }
      else update
    }
  }

  def render(in : NodeSeq) : NodeSeq = {
    if (query.isEmpty)
      <div class="error">Error: query does not exist</div>
    else
      helper.render(in, VotableQuery(query.get))
  }
}

class AddQuery extends StatefulSnippet {

  var dispatch : DispatchIt = { 
    case "create" => create _ 
    case "confirm" => confirm _
  }

  var what= "Frage"
  var keys= S.param("search").getOrElse("")

  def create(in:NodeSeq) : NodeSeq = {
    Helpers.bind("addquery", in,
		 "what" -> SHtml.textarea(what, what = _, "rows"-> "5", "cols"-> "20"),
		 "keys" -> SHtml.text(keys, keys = _, "size"-> "20"),
		 "submit" -> SHtml.submit("Absenden", ()=>verifyQuery))
  }

  def confirm(in:NodeSeq) : NodeSeq = {
    Helpers.bind("addquery", in,
		 "what" -> link("/add_query", {()=>}, Text(what)),
		 "keys" -> Markup.renderTagList(TaggedUtils.getTags(keys)),
		 "cancel" -> SHtml.submit("Abbrechen", ()=>redirectTo("/add_query")),
		 "submit" -> SHtml.submit("Absenden", ()=>saveQuery))
  }

  def verifyQuery() = {
    if (what.trim.length==0)
      redirectTo("/add_query")
    else
      redirectTo("/add_query_confirm")
  }

  def saveQuery() = {
    val query= 
      Query.create
      .what(what)
      .keys(keys)
      .creator(User.currentUser.get)
      .creation(Tick.now)
    query.save

    unregisterThisSnippet
    S.redirectTo("/queries/"+query.id+"/index.html")
  }
}

