package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import Helpers._

import _root_.org.liquidizer.lib.TaggedUtils

/** A discussed query */
object Query extends Query with LongKeyedMetaMapper[Query] {
  override def dbTableName = "queries"
  override def fieldOrder = List(what, creator, creation)
}

/** A discussed query */
class Query extends LongKeyedMapper[Query] with IdPK {
  def getSingleton = Query

  object what extends MappedString(this,1024)
  object creation extends MappedLong(this) 
  object creator extends MappedLongForeignKey(this, User)

  override def toString() : String = {
    what.is.toString
  }
}
