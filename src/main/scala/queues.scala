package Xactor

import Chisel._

abstract class Queue[T <: Data](val typ : T) {
  var name: String = ""
  def setName(name: String) {
    this.name = name
  }
}

class InQueue[T <: Data](typ: T) extends Queue(typ) {
}

class OutQueue[T <: Data](typ: T) extends Queue(typ) {
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
