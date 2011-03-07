package org.liquidizer.view

import scala.xml._
import scala.io._

import net.liftweb._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers.TheStrBindParam

import org.liquidizer.model._
import org.liquidizer.lib._

object EmotionView {

  val morpher= new Mesmerizer

  val sleeping= {
    val root="src/main/resources/"
    val src= scala.io.Source.fromFile(new java.io.File(root+"sleeping.svg"))
    scala.xml.parsing.XhtmlParser.apply(src).first
  }

  def doubleParam(id : String, default : Double) = 
    S.param(id).map { _.toDouble }.getOrElse(default)

  def face() = {
    val v= doubleParam("v", 0.5)
    val a= doubleParam("a", 0.5)
    val p= doubleParam("p", 0.5)
    val w= doubleParam("w", 1.0)
    val size= doubleParam("size", 100).toInt
    val scale= doubleParam("scale", 1.0)
    val view= S.param("view").getOrElse("front")

    var node= view match {
      case "front" => morpher.emoticon(v,a,p,w)
      case "sleeping" => sleeping
    }
 
    node= SVGUtil.resize(node, size, size, scale)
    
    Full(new XmlResponse(node, 200, "image/svg+xml", Nil) {
      // override the cache expiry
      override def headers = 
	TheStrBindParam("Cache-Control", 
			 "max-age= %d, public".format(60 * 60 * 24 * 7)) ::
			 super.headers
    })
 } 

  /** Create an embed tag for an inclusion of the emoticon */
  def emoticon(other : User, attribs:MetaData) : Node = {
    val size={attribs.get("size").getOrElse(Text("100"))}
    var uri= "/emoticons/face.svg" + {
      attribs.asAttrMap ++ {
	User.currentUser match {
	  case Full(me) => {
	    // compute face size based on distance metrics
	    val maxPref= VoteCounter.getMaxDelegation(me)
	    val dist= if (other==me) 1.0 else {
	      val w= Math.sqrt(VoteMap.getWeight(me, VotableUser(other)))
		1.0 + 0.2*(w - 0.5)*(maxPref min 3)
	    }
	    val fdist= SVGUtil.format(dist min 1.25)

	    // extract corresponding emotion
	    VoteCounter.getEmotion(me, other) match {
	    case Some(emo) => {
	      val p= emo.potency.is
	      val v= Math.pow(emo.valence.is / (.9*p + .1) / 2.0 + 0.5, 2.0)
	      val a= emo.arousal.is min 1.0 max 0.
	      val w= VoteMap.getCurrentWeight(other)
	      Map("v" -> SVGUtil.format(v), 
		  "a" -> SVGUtil.format(a), 
		  "p" -> SVGUtil.format(p),
		  "w" -> SVGUtil.format(w),
		  "scale" -> fdist)
	    }
	    case None => Map("view" -> "sleeping", "scale" -> fdist)
	  }}
	  case _ => Map("view" -> "sleeping")
	}}
    }.map { case (a,b) => a+"="+b }.mkString("?","&","")

    // The HTML tag
    <embed alt="Emoticon" src={uri} width={size} height={size}/>
  }

}
