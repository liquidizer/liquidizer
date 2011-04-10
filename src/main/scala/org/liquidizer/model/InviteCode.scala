package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class InviteCode extends LongKeyedMapper[InviteCode] with IdPK {
  def getSingleton = InviteCode

  object user extends MappedLongForeignKey(this, User)
  object code extends MappedString(this,255) with DBIndexed

}

object InviteCode extends InviteCode with LongKeyedMetaMapper[InviteCode] {
  override def dbTableName = "invitations"
  override def fieldOrder = List(code, user)

  def get(code : String) : Option[InviteCode] = find(By(this.code, code))
}

