package org.liquidizer.view

import scala.xml._
import scala.io._

import net.liftweb._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers.TheStrBindParam

object EmotionView {

  val morpher= new Mesmerizer

  val sleeping= {
    val root="src/main/resources/"
    val src= scala.io.Source.fromFile(new java.io.File(root+"sleeping.svg"))
    scala.xml.parsing.XhtmlParser.apply(src).first
  }

  def face() = {
    val v= S.param("v").getOrElse("0.5").toDouble
    val a= S.param("a").getOrElse("0.5").toDouble
    val p= S.param("p").getOrElse("0.5").toDouble
    val size= S.param("size").getOrElse("100").toInt
    val view= S.param("view").getOrElse("front")

    var node= view match {
      case "front" => morpher.emoticon(v,a,p)
      case "sleeping" => sleeping
    }
 
    node= SVGUtil.resize(node, size, size)
    
    Full(new XmlResponse(node, 200, "image/svg+xml", Nil) {
      // override the cache expiry
      override def headers = 
	TheStrBindParam("Cache-Control", 
			 "max-age= %d, public".format(60 * 60 * 24 * 7)) 
			 :: super.headers
    })
 } 
}
