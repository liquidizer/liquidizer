package org.liquidizer.lib.bpt

import java.io.File
import scala.xml._

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.model._
import org.liquidizer.lib._

object StartUp {

  def load(file:String) : Node = {
    val root=""
    val src= scala.io.Source.fromFile(new File(root+file))
    val doc= scala.xml.parsing.XhtmlParser.apply(src)
    doc.first
  }
  
  def run() = {
    val queries= load("queries.xml")
    for (query <- queries \ "query") {
      val text= query.child.text.trim
      if (Query.find(By(Query.what, text)).isEmpty) {
	println("Adding query: "+ text)
	val obj= Query.create.what(text).creation(Tick.now)
	obj.save
	Votable.create.query(obj).save
      }
    }

    val codes= load("invitations.xml")
    for (code <- codes \ "code") {
      val text= code.child.text.trim
      if (InviteCode.get(text).isEmpty) {
	println("Adding invitation code: "+text)
	InviteCode.create.code(text).save
      }
    }
  }
}
