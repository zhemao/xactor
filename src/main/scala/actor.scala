package Xactor

import Chisel._

abstract class Queue[T <: Data](val typ : T)

class InQueue[T <: Data](typ: T) extends Queue(typ) {
}

class OutQueue[T <: Data](typ: T) extends Queue(typ) {
  def <== (x: T) {
  }
}

object InQueue {
  def apply[T <: Data](typ: T) = {
    new InQueue(typ)
  }
}

object OutQueue {
  def apply[T <: Data](typ: T) = {
    new OutQueue(typ)
  }
}

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
  def := (x: T) {
  }

  def value = {
    init
  }
}

class Actor {
  def action[T <: Data](queue: InQueue[T]) (func: T => Unit) {
  }

  def guard(cond: Bool) (func: => Unit) {
  }
}
