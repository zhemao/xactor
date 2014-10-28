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
  private val inputDepList = new HashMap[String,ArrayBuffer[Int]]
  private val outputDepList = new HashMap[String,ArrayBuffer[Int]]
  private val stateDepList = new HashMap[String,ArrayBuffer[Int]]
  private val scheduler = new Scheduler
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

  def generateSchedule {
    scheduler.registerDepList(inputDepList, "input")
    scheduler.registerDepList(outputDepList, "output")
    scheduler.registerDepList(stateDepList, "state")
    scheduler.generateDot
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
        guardMap(name) = new ArrayBuffer[(Bool,Bits)]
        inputDepList(name) = new ArrayBuffer[Int]
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
        outputDepList(name) = new ArrayBuffer[Int]
      }

      val stateregs = new ArrayBuffer[(String,Data)]
      for (state <- states) {
        val reg = Reg(state.typ, init=state.init)
        state.setReg(reg)
        stateregs += Tuple2(state.name, reg)
        guardMap(state.name) = new ArrayBuffer[(Bool,Bits)]
        stateDepList(state.name) = new ArrayBuffer[Int]
      }

      val curSchedule = UInt(width = actions.length)
      val schedulerAddr = Vec.fill(actions.length){Bool()}

      for ((act,i) <- actions.view.zipWithIndex) {
        //Invoke each action to generate requiste logic
        val dact = act.asInstanceOf[Action[Data,Data]]
        val ind = portMap(dact.inqueue.name)
          .asInstanceOf[DecoupledIO[Data]]
        inputDepList(dact.inqueue.name) += i
        val res = dact.func(ind.bits)
        var fullguard = lastguard && ind.valid
        if(dact.outqueue != null) {
          val outd = portMap(dact.outqueue.name)
            .asInstanceOf[DecoupledIO[Data]]
          fullguard = fullguard && outd.ready
          //Register current actors output dependency 
          outputDepList(dact.outqueue.name) += i
        }
        //Create the lookup address with evaluated guards
        schedulerAddr(i) := fullguard

        //Zip data, with scheduled predicate to drive priority muxes
        if (dact.outqueue != null) {
          guardMap(dact.outqueue.name) += Tuple2(curSchedule(i), res.toBits)
        }
        if (lastupdate._2 != null) {
          val (name, data) = lastupdate
          stateDepList(name) += i
          guardMap(name) += Tuple2(curSchedule(i), data.toBits)
        }
        guardMap(dact.inqueue.name) += Tuple2(curSchedule(i), null)
        lastguard = Bool(true)
        lastupdate = ("", null)
      }

      generateSchedule

      val schedule = new ArrayBuffer[UInt]
      for (i <- 0 until (1 << actions.length)){
        schedule += UInt(i, width = actions.length)
      }
      val scheduleROM = Vec(schedule)
      curSchedule := scheduleROM(schedulerAddr.toBits)

      for ((name, _) <- outputPorts) {
        val outd = portMap(name).asInstanceOf[DecoupledIO[Data]]
        outd.bits := PriorityMux(guardMap(name))
        outd.valid := guardMap(name).unzip._1.reduce(_ || _)
      }

      for ((name, _) <- inputPorts) {
        val ind = portMap(name).asInstanceOf[DecoupledIO[Data]]
        ind.ready := guardMap(name).unzip._1.reduce(_ || _)
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
