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
    actor.addUpdate(name, x)
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

object StateArray {
  def apply[T <: Data](elts: Iterable[State[T]]): StateArray[T] = {
    new StateArray(elts.toArray)
  }

  def apply[T <: Data](elt0: State[T], elts: State[T]*): StateArray[T] = {
    apply(elt0 +: elts.toSeq)
  }

  def fill[T <: Data](n: Int)(gen: => State[T]): StateArray[T] = {
    tabulate(n){ i => gen }
  }

  def tabulate[T <: Data](n: Int)(gen: Int => State[T]): StateArray[T] = {
    apply((0 until n).map(gen))
  }
}

class StateArray[T <: Data](val elts: Array[State[T]]) {
  private var actor: Actor = null
  var name = ""
  var vec: Vec[T] = null

  def setActor(actor: Actor) {
    this.actor = actor
    for ((state, i) <- elts.zipWithIndex) {
      state.setActor(actor)
    }
  }

  def setName(name: String) {
    this.name = name
    for ((state, i) <- elts.zipWithIndex) {
      state.setName(name + "_" + i)
    }
  }

  def setVec(vec: Vec[T]) {
    this.vec = vec
  }

  def size = elts.size

  def apply(i: Int): State[T] = {
    elts(i)
  }

  def read(x: UInt): T = {
    vec(x)
  }

  def write(idx: UInt, data: T) {
    if (actor == null)
      throw new Exception("actor has not been set yet")
    actor.addWrite(name, idx, data)
  }

  def idx_name = name + "__i"
  def update_name = name + "__u"
}
