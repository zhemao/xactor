package Xactor

import Chisel._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

case class Action[T <: Data, U <: Data](
    val func: T => U,
    val inqueue: InQueue[T],
    val outqueue: OutQueue[U])

abstract class ActorModule extends Module {
  val io = new Bundle
  val portMap = new HashMap[String,DecoupledIO[Data]]
}

class Actor {
  val inputPorts = new ArrayBuffer[(String,Data)]
  val outputPorts = new ArrayBuffer[(String,Data)]
  var inspected = false
  val actions = new ArrayBuffer[Action[_ <: Data, _ <: Data]]
  var lastguard = Bool(true)
  var lastupdate: (String,Data) = ("", null)
  val states = new ArrayBuffer[State[Data]]

  def setLastUpdate(name: String, data: Data) {
    lastupdate = Tuple2(name, data)
  }

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
              dstate.setActor(this)
              dstate.setName(name)
              states += dstate
            }
            case any => ()
          }
        }
      }
      inspected = true
    }
  }

  def toMod: ActorModule = {
    inspectStateElements

    val guardMap = new HashMap[String,ArrayBuffer[(Bool,Bits)]]
    val mod = Module(new ActorModule {
      for ((name, typ) <- inputPorts) {
        val port = new DecoupledIO(typ.clone).flip
        port.setName(name)
        port.bits.setName("io_" + name + "_bits")
        port.valid.setName("io_" + name + "_valid")
        port.ready.setName("io_" + name + "_ready")
        io += port
        portMap(name) = port
      }
      for ((name, typ) <- outputPorts) {
        val port = new DecoupledIO(typ.clone)
        port.setName(name)
        port.bits.setName("io_" + name + "_bits")
        port.valid.setName("io_" + name + "_valid")
        port.ready.setName("io_" + name + "_ready")
        io += port
        portMap(name) = port
        guardMap(name) = new ArrayBuffer[(Bool,Bits)]
      }

      val stateregs = new ArrayBuffer[(String,Data)]
      for (state <- states) {
        val reg = Reg(state.typ, init=state.init)
        state.setReg(reg)
        stateregs += Tuple2(state.name, reg)
        guardMap(state.name) = new ArrayBuffer[(Bool,Bits)]
      }

      for (act <- actions) {
        val dact = act.asInstanceOf[Action[Data,Data]]
        val ind = portMap(dact.inqueue.name)
          .asInstanceOf[DecoupledIO[Data]]
        val res = dact.func(ind.bits)
        var fullguard = lastguard && ind.valid

        if (dact.outqueue != null) {
          val outd = portMap(dact.outqueue.name)
            .asInstanceOf[DecoupledIO[Data]]
          fullguard = fullguard && outd.ready
          guardMap(dact.outqueue.name) += Tuple2(fullguard, res.toBits)
        }
        if (lastupdate._2 != null) {
          val (name, data) = lastupdate
          guardMap(name) += Tuple2(fullguard, data.toBits)
        }
        lastguard = Bool(true)
        lastupdate = ("", null)
      }

      for ((name, typ) <- outputPorts) {
        val outd = portMap(name)
          .asInstanceOf[DecoupledIO[Data]]
        val outMux = PriorityMux(guardMap(name))
        val outValid = guardMap(name).unzip._1.reduce(_ || _)
        outd.bits := Reg(next = outMux)
        outd.valid := Reg(next = outValid)
      }

      for ((name, reg) <- stateregs) {
        val regMux = PriorityMux(guardMap(name))
        val regEn = guardMap(name).unzip._1.reduce(_ || _)
        when (regEn) {
          reg := regMux
        }
      }
    })
    mod
  }
}
