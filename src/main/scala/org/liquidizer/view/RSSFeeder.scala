package org.liquidizer.view

import scala.xml.{NodeSeq,Node,Text}
import java.text.SimpleDateFormat
import java.util.Date

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.provider._
import net.liftweb.mapper._

import org.liquidizer.snippet.InRoom
import org.liquidizer.model._
import org.liquidizer.lib._

object RSSFeeder {
val xmlHead = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"

  def feedAsArray = (xmlHead + (new RSSData()).createFeed.toString)
    .getBytes("UTF-8")

  def renderRSS : Box[LiftResponse] = {
    returnAsFeed(feedAsArray)
  }

  private def headers(length: String) = {
    ("Content-type" -> "application/rss+xml; charset=utf-8") ::
    ("Content-length" -> length) :: Nil
  }

  def returnAsFeed(in: Array[Byte]): Box[LiftResponse] = {
    Full(StreamingResponse(
      new java.io.ByteArrayInputStream(in),
      () => {},
      in.length,
      headers(in.length.toString), Nil, 200)
       )
  }
}

class RSSData extends InRoom {

  var index= 0

  def getData() : List[Query] = {
    val data = Votable.findAll(By_>(Votable.query, 0), 
			       By(Votable.room, room.get))
    val quotes= data.map { v => (v, VoteMap.getCurrentResult(v).value) }
    val rank= Map(quotes :_* )

    data
    .sort { rank(_) > rank(_) }
    .map { _.query.obj.get }
  }

  val dformat= new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z")
  def dateStr() = dformat.format(new Date())

  def createFeed = {
    <rss version="2.0">
    <channel>
    <title>Liquidizer RSS Feed</title>
    <link>http://liquidizer.org</link>
    <description>Liqidizer</description>
    <language>de-de</language>
    <pubDate>{ dateStr }</pubDate>
    <lastBuildDate>{ dateStr }</lastBuildDate>
    <ttl>10</ttl>
    <link xmlns="http://www.w3.org/2005/Atom" 
          href={ S.hostAndPath+"/room/1/feed.rss" } rel="self"
          type="application/rss+xml"/>
    {
      getData().slice(0,10).map { entry =>
      (<item>
       <title>{ 
	 index += 1
	 Text(index.toString+". "+entry.toString)
       }</title>
       <description></description>
       <link>{ S.hostAndPath+uri(entry) }</link>
       <guid isPermaLink="true">{ S.hostAndPath+uri(entry) }</guid>
       </item>)
	      }
    }
    </channel>
    </rss>
  }
}
