package Xactor

import Chisel._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

case class Action[T <: Data, U <: Data](
    val func: T => U,
    val inqueue: InQueue[T],
    val outqueue: OutQueue[U])

class Actor {
  val inputPorts = new ArrayBuffer[(String,Data)]
  val outputPorts = new ArrayBuffer[(String,Data)]
  val reglist = new ArrayBuffer[Data]
  var inspected = false
  val actions = new ArrayBuffer[Action[_ <: Data, _ <: Data]]
  var lastguard = Bool(true)

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
              inputPorts += Tuple2(name, queue.typ)
            }
            case queue: OutQueue[_] => {
              queue.setName(name)
              outputPorts += Tuple2(name, queue.typ)
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
    
    val guardMap = new HashMap[String,ArrayBuffer[(Bool,Bits)]]
    val mod = Module(new Module {
      val io = new Bundle
      val portMap = new HashMap[String,Data]
      for ((name, typ) <- inputPorts) {
        val port = new DecoupledIO(typ.clone).flip
        port.setName(name)
        io += port
        portMap(name) = port
      }
      for ((name, typ) <- outputPorts) {
        val port = new DecoupledIO(typ.clone)
        port.setName(name)
        io += port
        portMap(name) = port
        guardMap(name) = new ArrayBuffer[(Bool,Bits)]
      }

      val stateregs = new HashMap[String,Data]
      for (reg <- reglist) {
        stateregs(reg.name) = reg
      }

      for (act <- actions) {
        val dact = act.asInstanceOf[Action[Data,Data]]
        if (dact.outqueue != null) {
          val ind = portMap(dact.inqueue.name)
            .asInstanceOf[DecoupledIO[Data]]
          val outd = portMap(dact.outqueue.name)
            .asInstanceOf[DecoupledIO[Data]]
            
          val res = dact.func(ind.bits).toBits
          guardMap(dact.outqueue.name) += Tuple2(
            (lastguard && ind.valid && outd.ready), res)   
          lastguard = Bool(true)
        }
      }
      
      for ((name, typ) <- outputPorts) {
        val outd = portMap(name)
          .asInstanceOf[DecoupledIO[Data]]
        val outMux = PriorityMux(guardMap(name)) 
        outd.bits := outMux
        outd.valid := guardMap(name).unzip._1.reduce(_ || _) 
       }
    })
    mod
  }
}
