package org.liquidizer.lib.bpt

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.model._
import org.liquidizer.lib._

class BPTQueryOrder {

  val queries= Votable.findAll(By_>(Votable.query, 0))

  var resultMap= Map(queries.map { q => (q, VoteMap.getCurrentResult(q).value) }:_*)

  var subblock=0.0
  var blockMap= Map[String, Double]()
  for (query <- queries) {
    for (key <- query.query.obj.get.keyList() if key.startsWith("#block")) {
      val old= blockMap.get(key).getOrElse(0.0)
      val value = old max resultMap.get(query).get
      subblock= subblock + 1e-9
      blockMap += key -> (value + subblock)
    }
  }
  
  
  resultMap = resultMap.map { case (nominee, value) => {
    val blockValue=
      nominee.query.obj.get.keyList()
    .map { blockMap.get(_).getOrElse(0.0) }
    .foldLeft(value) { _ max _ }
    
    nominee -> (blockValue + value * 1e-11)
  }}

  def getWeight(n : Votable) : Double = {
    resultMap.get(n).getOrElse(0.0)
  }
}