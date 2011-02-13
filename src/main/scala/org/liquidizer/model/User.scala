package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

/**
 * The singleton that has methods for accessing the database
 */
object User extends User
	with LongKeyedMetaMapper[User]
	with MetaMegaProtoUser[User]  {
	  
	  override def dbTableName = "users"

	  override def signupFields = nick :: email :: password :: Nil
	  override def fieldOrder = List(id, email, nick, profile, password, validated)

	  // comment this line out to require email validations
	  override def skipEmailValidation = true
	}

class User extends LongKeyedMapper[User] 
with MegaProtoUser[User] {
  
  def getSingleton = User

  object nominee extends MappedLongForeignKey(this, Votable)
  object nick extends MappedString(this,32)
  object profile extends MappedText(this)

  def getUser(id : String) : Option[User] = User.find(By(User.id,id.toLong))
  
  def getUserByNick(nick : String) : Option[User] = {
    User.find(By(User.nick, nick))
  }

  def createNominee() : Unit = {
    val n= Votable.create.user(this)
    n.save
    nominee(n)
    save
  }

  override def toString() : String = nick.is
}
