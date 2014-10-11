package Xactor

import Chisel._
import scala.collection.mutable.ArrayBuffer

abstract class Queue[T <: Data](val typ : T)

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
  def action[T <: Data, U <: Data](
      inqueue: InQueue[T], outqueue: OutQueue[U]) (func: T => U) {
  }

  def action[T <: Data](inqueue: InQueue[T]) (func: T => Unit) {
  }

  def guard[T](cond: Bool) (func: => T): T = {
    func
  }

  def toMod: Module = {
    val methods = getClass.getDeclaredMethods.sortWith {
      (x, y) => (x.getName < y.getName)
    }
    val members = new ArrayBuffer[Data]
    for (m <- methods) {
      val numparams = m.getParameterTypes.length
      val rtype = m.getReturnType
      val isQueue = classOf[Queue[_]].isAssignableFrom(rtype)

      if (numparams == 0 && isQueue) {
        val name = m.getName
        val obj = m.invoke(this)
        obj match {
          case queue: InQueue[_] => {
            val port = new DecoupledIO(queue.typ.clone).flip
            port.setName(name)
            members += port 
          }
          case queue: OutQueue[_] => {
            val port = new DecoupledIO(queue.typ.clone)
            port.setName(name)
            members += port 
          }
          case any => ()
        }
      }
    }
    Module(new Module {
      val io = new Bundle
      for (data <- members) {
        io += data
      }
    })
  }
}
