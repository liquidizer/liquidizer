package org.liquidizer.lib

import net.liftweb.mapper._
import scala.collection.mutable
import org.liquidizer.model._

object TaggedUtils {

  /** Pattern tomatch a hash tag in a comment */
  val HASHTAG_R= "#(\\p{L}|\\d)+".r

  /** Extract all hash keys #..... from a string */
  def tagList(content : String) = {
    HASHTAG_R.findAllIn(content).toList.removeDuplicates
  }

  /** Get the most common tags for a room */
  def tagList(room : Room) : List[String] = {
    val list= Votable.findAll(By(Votable.room, room),By_>(Votable.query,0))
    val util= new TaggedUtil()
    util.load(list)
    util.sortedTags(list)
  }

  /** Extract the key list from a nominee, as displayed online */
  def keyList(nominee : Votable) = {
    nominee match {
      case VotableUser(user) => 
	tagList(user.profile.is)
      case _ => 
	val util = new TaggedUtil()
	util.load(List(nominee))
        util.keyList(nominee)
    }
  }
}

class TaggedUtil {
  var rating = Map[Votable, List[(String,Double)]]()

  def sortedTags(data : List[Votable]) : List[String] = {
    var total= Map[String, (String, Double)]()
    for (nominee <- data) {
      val tags= rating.get(nominee).getOrElse(List())
      for (tag <- tags) {
	val weight= tag._2
	val ltag= tag._1.toLowerCase
	val oval= total.get(ltag).map { _._2 }.getOrElse(0.0)
	total += ltag -> (tag._1, 1.0 + oval)
      }
    }
    total.toList.sort { _._2._2 > _._2._2 }.map { _._2._1 }
  }

  /** Extract the sorted list of tags for this */
  def keyList(nominee : Votable) : List[String] = {
    rating.get(nominee).getOrElse(List()).sort { _._2 > _._2 }.map { _._1 }
  }

  /** Extract hash tags from comments and assign them to their nominees */
  def hashTags(comments : List[Comment], nominee : Votable) {
    var rates = rating.get(nominee).getOrElse(List())
    for (comment <- comments) {
      lazy val author= comment.author.obj.get
      lazy val weight= Math.sqrt(VoteMap.getWeight(author, nominee).abs)
      val keys= TaggedUtils.tagList(comment.content.is)
      val factor= 1.0/ keys.size
      var offset= 1.0
      // tiny offset to retain tag order
      for (key <- keys) {
	rates ::= (key, factor*weight + offset * 1e-5)
	offset/= 2.0
      }
    }
    // sum weights from all comments
    var map= Map[String, Double]()
    while (!rates.isEmpty) {
      val rlist= rates.filter { _._1.equalsIgnoreCase(rates.head._1) }
      map += rates.head._1 -> rlist.foldLeft(0.) { _ + _._2 }
      rates= rates -- rlist
    }
    rating += nominee -> map.toList
  }

  /** Extract the tags from comments */
  def load(list : List[Votable]) {
    val comments= Comment.findAll(ByList(Comment.nominee, list.map{_.id.is}))
    for (n <- list)
      hashTags(comments.filter( _.nominee.is == n.id.is ), n)
  }
}
