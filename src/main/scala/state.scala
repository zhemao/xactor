package Xactor

import Chisel._

object State {
  def apply[T <: Data](typ: T = null, init: T = null) = {
    val mType = if (typ != null) {
      typ
    } else if (init != null) {
      init
    } else {
      throw new Exception("could not infer type of state")
    }

    new State(mType, init)
  }
}

class State[T <: Data](val typ: T, val init: T) {
  private var actor: Actor = null
  var name = ""
  var reg: Data = null

  def := (x: T) {
    if (actor == null)
      throw new Exception("Actor has not yet been set")
    actor.setLastUpdate(name, x)
  }

  def value = reg.asInstanceOf[T]

  def setActor(actor: Actor) {
    this.actor = actor
  }

  def setName(name: String) {
    this.name = name
  }

  def setReg(reg: Data) {
    this.reg = reg
  }
}
