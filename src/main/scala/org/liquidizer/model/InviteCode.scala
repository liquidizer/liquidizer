package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class InviteCode extends LongKeyedMapper[InviteCode] with IdPK {
  def getSingleton = InviteCode

  object owner extends MappedLongForeignKey(this, User)
  object room extends MappedLongForeignKey(this, Room)
  object code extends MappedString(this,255)

}

object InviteCode extends InviteCode with LongKeyedMetaMapper[InviteCode] {
  override def dbTableName = "invite_codes"

  def get(id : Long) : Option[InviteCode] = find(By(InviteCode.id, id))
}

