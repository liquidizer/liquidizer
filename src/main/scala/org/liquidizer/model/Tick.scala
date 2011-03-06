package org.liquidizer.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

case class Quote(pro:Double, contra:Double) {
  def /(denom : Int):Quote = if (denom==0) Quote(0.0, 0.0) else Quote(pro/denom, contra/denom)
  def +(quote : Quote):Quote = Quote(pro+quote.pro, contra+quote.contra)
  def *(factor: Double):Quote= Quote(pro*factor, contra*factor)
  def value : Double = pro - contra
  def volume : Double = pro + contra
}

class Tick extends LongKeyedMapper[Tick] with IdPK {
  def getSingleton = Tick

  object time extends MappedLong(this)
  object pro extends MappedDouble(this)
  object contra extends MappedDouble(this) 
  object votable extends MappedLongForeignKey(this, Votable) with DBIndexed
 
  // Conversions to Quote
  def quote() = Quote(pro.is, contra.is)
  def quote(q : Quote) = pro(q.pro).contra(q.contra)
}

object Tick extends Tick with LongKeyedMetaMapper[Tick] {
  override def dbTableName = "ticks"

  val df = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  def format(time:Long) = df.format(new java.util.Date(time))
  def toQuote(value : Double) = Quote(Math.max(value,0), Math.max(-value,0))

  def now = System.currentTimeMillis

  val sec= 1000L
  val min= 60*sec
  val h= 60*min
  val day= 24*h

  /** Get the timeseries for a certain votable */
  def getTimeSeries(nominee : Votable) : List[Tick] =
    findAll(By(votable, nominee), OrderBy(time, Descending))

  /** Compress the time series by coarsening the resolution for older quotes */
  def compress(ts : List[Tick]) = {
    if (!ts.isEmpty) {
      val t0= ts.head.time.is
      var prev : Option[Tick] = None
      var space= List(1*min, 5*min, 10*min, 20*min, 1*h, 2*h, 4*h, 12*h, 24*h)
      for (tick <- ts) {
	// coursen resultion for older ticks
	if (space.size>1 && t0-tick.time.is > 10*space.head) space=space.tail
	// lock to fixed time grid
	tick.time((tick.time.is / space.head)*space.head)
	// merge ticks
	if (!prev.isEmpty && prev.get.time.is == tick.time.is) {
	val newQuote= (prev.get.quote+tick.quote)*0.5
	  prev.get.quote(newQuote)
	  prev.get.save
	  tick.quote(newQuote)
	  tick.delete_!
	}
	prev= Some(tick)
      }
    }    
  }
}

