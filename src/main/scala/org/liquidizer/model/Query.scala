package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import Helpers._

import _root_.org.liquidizer.lib.TaggedUtils

object Query extends Query with LongKeyedMetaMapper[Query] {
	override def dbTableName = "queries"
	override def fieldOrder = List(what, creator, keys, creation)
  
  	def getQuery(id : String) : Option[Query] = getQuery(id.toLong)
  	def getQuery(id : Long) : Option[Query] = {
		Query.find(By(Query.id, id))
	}
}

class Query extends LongKeyedMapper[Query] with IdPK {
	def getSingleton = Query

	object what extends MappedString(this,255)
	object keys extends MappedString(this,255)
	object creation extends MappedLong(this) 
	object creator extends MappedLongForeignKey(this, User)

	override def toString() : String = {
		what.is.toString
	}
 
	def keyList() : List[String] = {
	       TaggedUtils.getTags(keys.is)
	}
}
