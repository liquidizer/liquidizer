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

  override def categories(in:NodeSeq) : NodeSeq = {
    val markup= new CategoryView(search, "/queries.html")
    <span>{
      markup.renderTagList(TaggedUtils.sortedTags(getData()), 10)
    }</span>
  }

  def render(in: NodeSeq) : NodeSeq = {
    val helper= new VotingHelper
    helper.no= from
    getData()
    .slice(from, to)
    .flatMap {
      item =>  helper.bind(in, item)
    }
  }
}

class QueryDetails extends MultipageSnippet {
  val query= Query.getQuery(S.param("query").openOr("-1"))
  var hasMe= true;

  def loadData() = {
    data= 
      VoteCounter.getAllVoters(query.get)
    .filter { searchFilter _ }
    .map { VotableUser(_) }

    sortData(VotableQuery(query.get))

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
      helper.bind(in, VotableQuery(query.get))
  }
}

class AddQuery extends StatefulSnippet {

  var dispatch : DispatchIt = { 
    case "create" => create _ 
    case "confirm" => confirm _
  }

  var what= ""
  var keys= S.param("search").getOrElse("")

  def create(in:NodeSeq) : NodeSeq = {
    Helpers.bind("addquery", in,
		 "what" -> SHtml.textarea(what, what = _, "rows"-> "3", "cols"-> "40", "placeholder" -> S ? "new.query"),
		 "keys" -> SHtml.text(keys, keys = _, 
				      "size"-> "40", "id" -> "keys"),
		 "submit" -> SHtml.submit(S ? "new.query.confirm", ()=>verifyQuery),
		 "keyset" -> 
		 new CategoryView(keys, "") {
		   override def renderTag(tag : String) : Node = {
		     <div class={getStyle(isActive(tag))} 
		     id={"key_"+tag}
		     onclick={"toggleTag('"+tag+"')"}>{tag}</div>
		   }
		 }.renderTagList(TaggedUtils.sortedQueryTags(), 20)
	       )
  }

  def confirm(in:NodeSeq) : NodeSeq = {
    Helpers.bind("addquery", in,
		 "what" -> Markup.renderHeader(what, link("/add_query", {()=>}, _)),
		 "keys" -> Markup.renderTagList(TaggedUtils.getTags(keys)),
		 "cancel" -> SHtml.submit(S ? "new.query.cancel", ()=>redirectTo("/add_query")),
		 "submit" -> SHtml.submit(S ? "new.query.confirm", ()=>saveQuery))
  }

  def isValid() = what.trim.length>0 && Query.find(By(Query.what, what)).isEmpty

  def verifyQuery() =
    if (isValid) redirectTo("/add_query_confirm") else redirectTo("/add_query")

  def saveQuery() = {
    if (isValid) {
      val query= 
	Query.create
      .what(what)
      .keys(keys)
      .creator(User.currentUser.get)
      .creation(Tick.now)
      query.save
      query.createNominee

      unregisterThisSnippet
      S.redirectTo("/queries/"+query.id+"/index.html")
    } else {
      what=""
      redirectTo("/add_query")
    }
  }
}

