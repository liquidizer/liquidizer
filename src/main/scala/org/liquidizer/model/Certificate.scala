package org.liquidizer.model

import net.liftweb._
import mapper._
import common.{Box, Full}

class Certificate extends KeyedMapper[String, Certificate] {
    def getSingleton = Certificate

    def primaryKeyField = id

    object id extends MappedString(this, 1024) with IndexedField[String] {
        override def dbNotNull_? = true
        override def writePermission_? = true

        def makeKeyJDBCFriendly(in: String) = in

        def convertKey(in: String): Box[String] = Box.legacyNullTest(in)
        def convertKey(in: Int): Box[String] = Full(in.toString)
        def convertKey(in: Long): Box[String] = Full(in.toString)
        def convertKey(in: AnyRef): Box[String] =
            Box.legacyNullTest(in).map(_.toString)
    }
    
    object owner extends MappedLongForeignKey(this, User)
}

object Certificate extends Certificate with KeyedMetaMapper[String, Certificate] {

    override def dbTableName = "certificates"

    def getFor(user: User) = {
        findAll(By(owner, user.id))
    }
}
