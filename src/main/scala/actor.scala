package Xactor

import Chisel._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

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
  val ports = new ArrayBuffer[Data]
  val reglist = new ArrayBuffer[Data]
  var inspected = false

  def action[T <: Data, U <: Data](
      inqueue: InQueue[T], outqueue: OutQueue[U]) (func: T => U) {
  }

  def action[T <: Data](inqueue: InQueue[T]) (func: T => Unit) {
  }

  def guard[T](cond: Bool) (func: => T): T = {
    func
  }

  def inspectStateElements {
    if (!inspected) {
      val methods = getClass.getDeclaredMethods.sortWith {
        (x, y) => (x.getName < y.getName)
      }
      for (m <- methods) {
        val numparams = m.getParameterTypes.length
        val rtype = m.getReturnType

        if (numparams == 0) {
          val name = m.getName
          val obj = m.invoke(this)
          obj match {
            case queue: InQueue[_] => {
              val port = new DecoupledIO(queue.typ.clone).flip
              port.setName(name)
              ports += port
            }
            case queue: OutQueue[_] => {
              val port = new DecoupledIO(queue.typ.clone)
              port.setName(name)
              ports += port
            }
            case state: State[_] => {
              val dstate = state.asInstanceOf[State[Data]]
              val reg = Reg(dstate.typ, init = dstate.init)
              reglist += reg
            }
            case any => ()
          }
        }
      }
      inspected = true
    }
  }

  def toMod: Module = {
    inspectStateElements
    
    Module(new Module {
      val io = new Bundle
      for (data <- ports) {
        io += data
      }

      val stateregs = new HashMap[String,Data]
      for (reg <- reglist) {
        stateregs(reg.name) = reg
      }
    })
  }
}
