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

/** Snippet code to show all discussed queries */
class Queries extends MultipageSnippet {
  
  /** load all queries form the current room */
  def getData() = {
    if (data.isEmpty && !room.isEmpty) {
      data = Votable.findAll(By_>(Votable.query, 0), By(Votable.room, room.get))
      tags.load(data)
      data= data.filter { searchFilter _ }
      sortData()
    }
    data
  }

  /** Render a list of category tags */
  override def categories(in:NodeSeq) : NodeSeq = {
    val markup= new CategoryView(search, "queries.html")
    <span>{
      markup.renderTagList(tags.sortedTags(data), 10)
    }</span>
  }

  /** Render a list of all queries */
  def render(in: NodeSeq) : NodeSeq = {
    val helper= new VotingHelper {
      override def getKeyTags(nominee : Votable) = 
	Text(tags.keyList(nominee).mkString(" "))
    }
    helper.no= from
    getData()
    .slice(from, to)
    .flatMap {
      item =>  helper.bind(in, item)
    }
  }
}

/** Snippet code to show details on a query's voters */
class QueryDetails extends MultipageSnippet with InRoom {
  val quid= S.param("query").get.toLong
  val query= Votable.getQuery(quid.toLong, room.get)
  var hasMe= true;

  def loadData() = {
    data=  Votable.get(
      VoteMap.getAllVoters(query.get)
      .filter{ u => searchFilter( u.toString ) }, room.get)
    sortData(query.get)

    // check if my own vote is registered. 
    // If not a page update should show it after the first vote is cast
    val me= User.currentUser
    hasMe= !me.isEmpty && data.exists { _.user.is == me.get.id.is }
  }

  val helper= new VotingHelper with MultiPageHelper {
    override def getData(src : String) = {
      if (data.isEmpty) loadData()
      data
    }
    override def ajaxUpdate(votedNominee : Votable) : JsCmd = {
      val update= super.ajaxUpdate(votedNominee)
      if (!hasMe && votedNominee==query.get) {
	hasMe= true
	update & RedirectTo(uri(query.get))
      }
      else update
    }
  }

  def render(in : NodeSeq) : NodeSeq = {
    if (query.isEmpty)
      <div class="error">Error: query does not exist</div>
    else
      helper.bind(in, query.get)
  }
}

class AddQuery extends StatefulSnippet with InRoom {

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
		 "submit" -> SHtml.submit(S ? "new.query.confirm", ()=>
		   verifyQuery.getOrElse { redirectTo("add_query_confirm")} ),
		 "keyset" -> 
		 new CategoryView(keys, "") {
		   override def renderTag(tag : String) : Node = {
		     <div class={getStyle(isActive(tag))} 
		     id={"key_"+tag}
		     onclick={"toggleTag('"+tag+"')"}>{tag}</div>
		   }
		 }.renderTagList(TaggedUtils.tagList(room.get), 20)
	       )
  }

  def confirm(in:NodeSeq) : NodeSeq = {
    keys= keys.split("[, ]").mkString(", ")
    Helpers.bind("addquery", in,
		 "what" -> Markup.renderHeader(what, link("/add_query", {()=>}, _)),
		 "keys" -> Text(keys),
		 "cancel" -> SHtml.submit(S ? "new.query.cancel", ()=>redirectTo("/add_query")),
		 "submit" -> SHtml.submit(S ? "new.query.confirm", ()=>saveQuery))
  }

  def verifyQuery() : Option[JsCmd] = {
    if (what.trim.length==0)
      redirectTo(home()+"/add_query")
    else {
      val exists= Query.findAll(By(Query.what, what))
      .map { q => Votable.find(By(Votable.query, q)) }
      .filter { _.exists { _.room.obj == room }}
      if (!exists.isEmpty) redirectTo(uri(exists.head.get))
      else 
	None
    }
  }

  def saveQuery() = {
    verifyQuery.getOrElse {
      val query= Query.create
      .what(what)
      .creator(User.currentUser.get)
      .creation(Tick.now)
      query.save
      val nominee=Votable.create
      .query(query)
      .room(room.get)
      nominee.save
      if (keys.trim.length>0)
	PollingBooth.comment(User.currentUser.get, nominee, 
			     keys.split("[, ]").mkString("#"," #",""))
      unregisterThisSnippet
      S.redirectTo(uri(query))
    }
  }
}

