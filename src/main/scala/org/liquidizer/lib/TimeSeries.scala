package org.liquidizer.lib

import _root_.net.liftweb.mapper._

import scala.collection._
import org.liquidizer.model._

case class Quote(pro:Double, contra:Double) {
  def /(denom : Int):Quote = if (denom==0) Quote(0.0, 0.0) else Quote(pro/denom, contra/denom)
  def +(quote : Quote):Quote = Quote(pro+quote.pro, contra+quote.contra)
  def *(factor: Double):Quote= Quote(pro*factor, contra*factor)
  def value : Double = pro-contra
  def volume : Double = pro+contra
}

case class Tick[A](time:Long, value:A) {
  override def toString():String = Tick.df.format(time)+" -> "+value
}

object Tick {
  val df = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  def format(time:Long) = df.format(new java.util.Date(time))
  def toQuote(value : Double) = Quote(Math.max(value,0), Math.max(-value,0))

  var offset= 0L
  def now = System.currentTimeMillis - offset

  val sec= 1000L
  val min= 60*sec
  val h= 60*min
  val day= 24*h
}

/** A time slice based data structure that keeps historic snapshots of values.
 The resolution is automatically decreased when time progresses to optimize performance.
 */
abstract class Slice[A](val stepSize:Long, val next:Option[Slice[A]]) {

  var list : List[Tick[A]] = Nil

  def baseTime(time : Long) = (time / stepSize) * stepSize
  def capTime(time : Long) = (time / next.get.stepSize - 5) * next.get.stepSize
  
  // this function is called when a slice drops out due to coarsed resolution
  def merge(oldVal:A, updateVal:A) : A
  
  def latestTime : Long = head.map { _.time }.getOrElse(0)
  def head : Option[Tick[A]] = {
    if (list.isEmpty)
      None
    else
      Some(list.head)
  }

  /** Returns true if the next data time stamp will not be merged immideately */
  def isNew(time:Long):Boolean = {
    list.isEmpty || (time > list.head.time + stepSize)
  }
  
  /** Add a new data slice to the list */
  def add(time:Long, data:A):Unit = {
    if (list.isEmpty || isNew(time)) {
      list ::= Tick(baseTime(time), data)
    } else {
      list = Tick(list.first.time, merge(list.head.value,  data)) :: list.tail
    }
    if (!next.isEmpty) {
      while (!list.isEmpty && list.last.time < capTime(time)) {
	next.get.add(list.last.time, list.last.value)
	list = list.init
      }
    }
  }
  
  def getAll(): List[Tick[A]] = {
    list ++ (if (next.isEmpty) Nil else next.get.getAll())
  }
  
  def dump():Unit = {
    list.foreach { println(_) }
    if (!next.isEmpty)
      next.get.dump()
  }
}

import Tick.{sec, min, h}
class TimeSeries(stepSize : Long, next : Option[TimeSeries]) extends Slice[Quote](stepSize, next) {
  def this(stepSizes : List[Long]) = {
    this(stepSizes.head, 
	 if (stepSizes.tail.isEmpty) None 
	 else Some(new TimeSeries(stepSizes.tail)))
  }
  def this() = this(List(10*sec, 60*sec, 5*min, 30*min, 60*min, 2*h, 4*h, 12*h, 24*h))
  
  def merge(a : Quote, b : Quote) : Quote = {
    if (stepSize<10000L) b else (a+b)*0.5
  }
}

