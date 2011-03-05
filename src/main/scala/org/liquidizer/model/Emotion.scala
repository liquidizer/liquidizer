package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class Emotion extends LongKeyedMapper[Emotion] with IdPK {
  def getSingleton = Emotion

  object time extends MappedLong(this)
  object valence extends MappedDouble(this)
  object potency extends MappedDouble(this) 
  object arousal extends MappedDouble(this) 
  object avg_valence extends MappedDouble(this)
  object avg_potency extends MappedDouble(this) 
  object user1 extends MappedLongForeignKey(this, User) with DBIndexed
  object user2 extends MappedLongForeignKey(this, User) with DBIndexed
  
  /** Set a new time value and compute the new arousal */
  def update(newTime : Long, decay : Double) = {
    if (newTime > time.is) {
      val factor= Math.exp( - decay * (newTime - time.is))
      avg_valence(factor * avg_valence.is + (1-factor) * valence.is)
      avg_potency(factor * avg_potency.is + (1-factor) * potency.is)
      time(newTime)
    }
    val swing1= valence.is - avg_valence.is
    val swing2= potency.is - avg_potency.is
    arousal((swing1.abs) max (swing2.abs))
  }
}

object Emotion extends Emotion with LongKeyedMetaMapper[Emotion] {
  override def dbTableName = "emotions"

  def get(u1 : User, u2 : User) : Emotion = {
    val id1= u1.id.is min u2.id.is    
    val id2= u1.id.is max u2.id.is
    find(By(user1, id1), By(user2, id2)).getOrElse (Emotion.create)
  }
}

