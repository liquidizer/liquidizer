package org.liquidizer.lib

class PoisonMemory(var value:Double, var time:Long) {

  val DECAY = 1e-9
  var memory= value

  def Double() : Double = value

  def this(value:Double) = this(value, -1)

  def set(newTime : Long, newVal : Double) : Unit = {
    update(newTime)
    value= newVal
  }

  def update(newTime:Long) : Unit = {
    val dt= newTime - time
    val factor= Math.exp(-DECAY*dt)
    memory= factor*memory + (1-factor)*value
    time= newTime
  }

  def smooth() = memory
  def swing() = Math.abs(memory-value)

  override def toString() = { value + "("+memory+")"}
}
