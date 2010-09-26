package org.liquidizer.view

import scala.xml._
import scala.collection.mutable

import _root_.org.liquidizer.lib._

class ResultCache[T] {
  val cache= mutable.Map.empty[(String, Map[String,String]), Tick[T]]
  val timeout= 30000
  var cacheTime= Tick.now

  def clear(time:Long) {
    cacheTime= time
    cache.clear
  }

  def get(key:String, options:Map[String,String]) : Option[T] = {
    VoteCounter.refresh
    val time= QuoteHistory.latestEntry
    if (time > cacheTime) {
      clear(time)
    }
    val result= cache.get((key, options))
    if (result.isEmpty) {
      None
    }
    else if (Tick.now - result.get.time > timeout) {
      cache.removeKey((key, options))
      None
    }
    else {
      Some(result.get.value)
    }
  }

  def get(key:String, options:Map[String, String], value:()=>T) : T = {
    var cacheVal= get(key,options)
    if (!cacheVal.isEmpty)
      cacheVal.get
    else
      put(key, options, value())
  }

  def put(key:String, options:Map[String,String], value:T) : T = {
    cache.put((key, options), Tick(Tick.now,value))
    value
  }
}

