package org.liquidizer.lib

import net.liftweb.http._

object TimeUtil {
  
  val sec= 1000L
  val min= 60*sec
  val h= 60*min
  val day= 24*h

  /** current Time */
  def now()= System.currentTimeMillis

  /** format a time relative to now */
  def formatRelTime(time : Long) : String = {
    val dt= now - time
    if (dt <= min) S.?("time.secs").format(dt/sec)
    else if (dt <= h) S.?("time.mins").format(dt/min)
    else if (dt < 2*h) S.?("time.one.hour")
    else if (dt <= day) S.?("time.hours").format(dt/h)
    else if (dt < 2*day) S.?("time.one.day")
    else S.?("time.days").format(dt/day)
  }
}
