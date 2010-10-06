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
  def size= queries.size

  val queries= {
    Query.findAll
    .filter { searchFilter _ }
//    .sort { sortOrder() }
  }
  
  override def categories(in:NodeSeq) : NodeSeq = {
    val keys= search.split(" +").toList.map { _ toLowerCase }
    val markup= new CategoryView(keys, "/queries.html")
    <span>{
      markup.renderTagList(TaggedUtils.sortedTags(queries), 5)
    }</span>
  }

  def render(in: NodeSeq) : NodeSeq = {
    val helper= new VotingHelper
    queries
    .slice(from,to)
    .flatMap {
      query =>  helper.render(in,VotableQuery(query))
    }
  }
}

class QueryDetails extends MultipageSnippet {
  val query= Query.getQuery(S.param("query").openOr("-1"))
  var supporters: List[User]= Nil

  def loadSupporters() = {
    println("load supporters")
    supporters= 
      VoteCounter.getAllVoters(query.get)
      .filter { searchFilter _ }
      //.sort { voteSortOrder(VotableQuery(query.get))  }
  }

  override def size= supporters.size

  val helper= new VotingHelper {
    override def getSupporters() : List[User] = {
      if (supporters.isEmpty)
	loadSupporters()
      supporters.slice(from,to)
    }
    override def ajaxUpdate(votedNominee : Votable) : JsCmd = {
      super.ajaxUpdate(votedNominee)
      //TODO: RedirectTo(nominee.uri)
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
		 "keys" -> SHtml.text(keys, keys = _, "cols"-> "20"),
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
    S.redirectTo("/queries/"+query.id)
  }
}

