package Xactor

import Chisel._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

case class Action[T <: Data, U <: Data](
    val func: T => U,
    val inqueue: InQueue[T],
    val outqueue: OutQueue[U])

class Actor {
  val inputPorts = new ArrayBuffer[Data]
  val outputPorts = new ArrayBuffer[Data]
  val reglist = new ArrayBuffer[Data]
  var inspected = false
  val actions = new ArrayBuffer[Action[_ <: Data, _ <: Data]]
  var lastguard: Bool

  def action[T <: Data, U <: Data](
      inqueue: InQueue[T], outqueue: OutQueue[U]) (func: T => U) {
    actions += Action(func, inqueue, outqueue)
  }

  def action[T <: Data](inqueue: InQueue[T]) (func: T => Unit) {
    val wrapfunc = (x: T) => {
      func(x)
      val typedNull: Data = null
      typedNull
    }
    actions += Action(wrapfunc, inqueue, null)
  }

  def guard[T](cond: Bool) (func: => T): T = {
    lastguard = cond
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
              queue.setName(name)
              val port = new DecoupledIO(queue.typ.clone).flip
              port.setName(name)
              outputPorts += port
            }
            case queue: OutQueue[_] => {
              queue.setName(name)
              val port = new DecoupledIO(queue.typ.clone)
              port.setName(name)
              inputPorts += port
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
    
    val guardMap = new HashMap[String,ArrayBuffer[(Bool,Data)]]
    val mod = Module(new Module {
      val io = new Bundle
      val portMap = new HashMap[String,Data]
      for (data <- inputPorts) {
        io += data
        portMap(data.name) = data
      }
      for (data <- outputPorts) {
        io += data
        portMap(data.name) = data
        guardMap(data.name) = new ArrayBuffer[(Bool,Data)]
      }

      val stateregs = new HashMap[String,Data]
      for (reg <- reglist) {
        stateregs(reg.name) = reg
      }

      for (act <- actions) {
        val ind = portMap(act.inqueue.name)
          .asInstanceOf[DecoupledIO[Data]]
        val outd = portMap(act.outqueue.name)
          .asInstanceOf[DecoupledIO[Data]]
          
        val res = act.func(ind.bits)
        guardMap(act.outqueue.name) += ((lastguard && ind.valid && outd.ready), res)   
        lastguard = Bool(True)
      }
      
      for (data <- outputPorts) {
        val outd = portMap(data.name)
          .asInstanceOf[DecoupledIO[Data]]
        val outMux = PriorityMux(guardMap(data.name)) 
        outd.bits := outMux.bits
        outd.valid := guardMap(data.name).unzip._1.reduce(_ || _) 
       }
    })
    mod
  }
}
