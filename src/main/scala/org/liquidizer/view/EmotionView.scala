package org.liquidizer.view

import scala.xml._

import net.liftweb._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._


object EmotionView {
  val morpher= new Mesmerizer
  def front() = {
    val v= S.param("valence").getOrElse("0").toDouble
    val a= S.param("arousal").getOrElse("0").toDouble
    val p= S.param("potency").getOrElse("0").toDouble
    val node= morpher.emoticon(v,a,p)

    Full(XmlResponse(node, "image/svg+xml"))
 } 
}
