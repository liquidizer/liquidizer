package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class Room extends LongKeyedMapper[Room] with IdPK {
  def getSingleton = Room

  object name extends MappedString(this,32)

  override def toString() = name.is
}

object Room extends Room with LongKeyedMetaMapper[Room] {
  override def dbTableName = "rooms"

  def get(id : Long) = find(By(this.id, id))
  def get(room : String) : Option[Room] = if (room.matches("[0-9]+"))
    get(room.toLong) else find(By(name, room))
}

