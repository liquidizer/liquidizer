package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

import org.liquidizer.lib.ssl._

/** Representation of a user of the Liquidizer system */
object User extends User
	with LongKeyedMetaMapper[User]
	with MetaMegaProtoUser[User]  {
	  
	  override def dbTableName = "users"

	  override def signupFields = nick :: email :: password :: Nil
	  override def fieldOrder = List(id, email, nick, profile, password, validated)

         // comment this line out to require email validations
         override def skipEmailValidation = true

         override def logUserIn(user: User) {
           for {
             val certs <- SSLClient.valid_certificates
               val cert <-
               certs if (Certificate.find(By(Certificate.id, SSLClient.certificate_id(cert))).isEmpty)
           } Certificate.create.id(SSLClient.certificate_id(cert)).owner(user).save
          super.logUserIn(user)
        }
}

/** Representation of a user of the Liquidizer system */
class User extends LongKeyedMapper[User] with MegaProtoUser[User] {
  
  def getSingleton = User

  object nick extends MappedString(this,32)
  object profile extends MappedText(this)

  def getUserByNick(nick : String) : Option[User] = {
    User.find(By(User.nick, nick))
  }

  override def toString() : String = nick.is
}
