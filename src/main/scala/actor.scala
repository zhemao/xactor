package Xactor

import Chisel._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

case class Action[T <: Data, U <: Data](
  val func: List[T] => List[U],
  val inqueues: List[InQueue[T]],
  val outqueues: List[OutQueue[U]])

abstract class ActorModule extends Module {
  val io = new Bundle
  val portMap = new HashMap[String,DecoupledIO[Data]]

  def connect[T <: Data](
      name: String, outerPort: DecoupledIO[T], qdepth: Int = 1) {
    val innerPort = portMap(name).asInstanceOf[DecoupledIO[T]]
    if (innerPort.valid.dir == INPUT) {
      val queue = Queue(outerPort, qdepth)
      innerPort <> queue
    } else {
      val queue = Queue(innerPort, qdepth)
      outerPort <> queue
    }
  }

  def connect(other: ActorModule, thisname: String, othername: String, qdepth: Int = 1) {
    val thisPort = portMap(thisname)
    val otherPort = other.portMap(othername)

    if (thisPort.valid.dir == INPUT)
      thisPort <> Queue(otherPort, qdepth)
    else
      otherPort <> Queue(thisPort, qdepth)
  }
}

class Actor {
  val inputPorts = new ArrayBuffer[(String,Data)]
  val outputPorts = new ArrayBuffer[(String,Data)]
  var inspected = false
  val actions = new ArrayBuffer[Action[_ <: Data, _ <: Data]]
  var lastguard = Bool(true)
  var lastupdates = new ArrayBuffer[(String,Data)]
  var lastwrites = new ArrayBuffer[(String,UInt,Data)]
  val states = new ArrayBuffer[State[Data]]
  val arrays = new ArrayBuffer[StateArray[Data]]
  private val inputDepList = new HashMap[String,ArrayBuffer[Int]]
  private val outputDepList = new HashMap[String,ArrayBuffer[Int]]
  private val stateDepList = new HashMap[String,ArrayBuffer[Int]]
  private val scheduler = new Scheduler

  def addUpdate(name: String, data: Data) {
    lastupdates += Tuple2(name, data)
  }

  def addWrite(name: String, idx: UInt, data: Data) {
    lastwrites += Tuple3(name, idx, data)
  }

  def action[T <: Data, U <: Data](
    inqueue: InQueue[T], outqueue: OutQueue[U]) (func: T => U) {
    val wrapfunc = (x: List[T]) => List(func(x.head))
    actions += Action(wrapfunc, List(inqueue), List(outqueue))
  }

  def action[T <: Data](inqueue: InQueue[T]) (func: T => Unit) {
    val wrapfunc = (x: List[T]) => {
      func(x.head)
      val typedNil: List[Data] = Nil
      typedNil
    }
    actions += Action(wrapfunc, List(inqueue), Nil)
  }

  def action[T <: Data, U <: Data](
    inqueues: List[InQueue[T]], outqueues: List[OutQueue[U]]) (func: List[T] => List[U]) {
    actions += Action(func, inqueues, outqueues)
  }

  def action[T <: Data](inqueues: List[InQueue[T]]) (func: List[T] => Unit) {
    val wrapfunc = (x: List[T]) => {
      func(x)
      val typedNil: List[Data] = Nil
      typedNil
    }
    actions += Action(wrapfunc, inqueues, Nil)
  }

  def action[T <: Data](outqueue: OutQueue[T]) (func: => T) {
    val wrapfunc = (x: List[_]) => List(func)
    actions += Action(wrapfunc, Nil, List(outqueue))
  }

  def action() (func: => Unit) {
    val wrapfunc = (x: List[_]) => {
      func
      val typedNil: List[Data] = Nil
      typedNil
    }
    actions += Action(wrapfunc, Nil, Nil)
  }

  def lguard[T](cond: Bool) (func: => List[T]): List[T] = {
    lastguard = cond
    func
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
            case array: StateArray[_] => {
              val darray = array.asInstanceOf[StateArray[Data]]
              darray.setName(name)
              darray.setActor(this)
              arrays += darray
            }
            case any => ()
          }
        }
      }
      inspected = true
    }
  }

  def generateSchedule : ArrayBuffer[UInt] = {
    scheduler.initialize(actions.length)
    scheduler.registerDepList(inputDepList, "input")
    scheduler.registerDepList(outputDepList, "output")
    scheduler.registerDepList(stateDepList, "state")
    scheduler.generateSchedule()
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
        reg.setName(state.name)
        state.setReg(reg)
        stateregs += Tuple2(state.name, reg)
        guardMap(state.name) = new ArrayBuffer[(Bool,Bits)]
        stateDepList(state.name) = new ArrayBuffer[Int]
      }

      val arrmap = new HashMap[String,StateArray[Data]]
      for (arr <- arrays) {
        val vec = Vec.tabulate(arr.size)(i => {
          val state = arr.elts(i)
          val reg = Reg(state.typ, init=state.init)
          reg.setName(state.name)
          state.setReg(reg)
          stateregs += Tuple2(state.name, reg)
          guardMap(state.name) = new ArrayBuffer[(Bool,Bits)]
          stateDepList(state.name) = new ArrayBuffer[Int]
          reg
        })
        vec.setName(arr.name)
        arr.setVec(vec)
        arrmap(arr.name) = arr
        guardMap(arr.idx_name) = new ArrayBuffer[(Bool,Bits)]
        guardMap(arr.update_name) = new ArrayBuffer[(Bool,Bits)]
      }

      val curSchedule = UInt(width = actions.length)
      val schedulerAddr = Vec.fill(actions.length){Bool()}

      for ((act,i) <- actions.view.zipWithIndex) {
        //Invoke each action to generate requiste logic
        val dact = act.asInstanceOf[Action[Data,Data]]

        for (inq <- dact.inqueues) {
          val ind = portMap(inq.name)
            .asInstanceOf[DecoupledIO[Data]]
          inputDepList(inq.name) += i
        }
        val inputs = dact.inqueues.map {
          inq => portMap(inq.name)
            .asInstanceOf[DecoupledIO[Data]].bits
        }
        val results = dact.func(inputs)

        var fullguard = lastguard
        for (inq <- dact.inqueues) {
          val ind = portMap(inq.name)
            .asInstanceOf[DecoupledIO[Data]]
          fullguard = fullguard && ind.valid
        }

        for (outq <- dact.outqueues) {
          val outd = portMap(outq.name)
            .asInstanceOf[DecoupledIO[Data]]
          fullguard = fullguard && outd.ready
          //Register current actors output dependency 
          outputDepList(outq.name) += i
        }
        //Create the lookup address with evaluated guards
        schedulerAddr(i) := fullguard

        //Zip data, with scheduled predicate to drive priority muxes
        for ((res, outq) <- results zip dact.outqueues) {
          guardMap(outq.name) += Tuple2(curSchedule(i), res.toBits)
        }
        for ((name, data) <- lastupdates) {
          stateDepList(name) += i
          guardMap(name) += Tuple2(curSchedule(i), data.toBits)
        }
        for ((aname, idx, data) <- lastwrites) {
          println(aname)
          val arr = arrmap(aname)
          for (j <- 0 until arr.size) {
            val sname = arr(j).name
            stateDepList(sname) += i
          }
          guardMap(aname + "__i") += Tuple2(curSchedule(i), idx)
          guardMap(aname + "__u") += Tuple2(curSchedule(i), data.toBits)
        }
        for (inq <- dact.inqueues) {
          guardMap(inq.name) += Tuple2(curSchedule(i), null)
        }
        lastguard = Bool(true)
        lastupdates = new ArrayBuffer[(String, Data)]
        lastwrites = new ArrayBuffer[(String,UInt,Data)]
      }

      val scheduleROM = Vec(generateSchedule)
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

      for (arr <- arrays) {
        val addrMux = PriorityMux(guardMap(arr.idx_name))
        val nextMux = PriorityMux(guardMap(arr.update_name))
        val writeEn = guardMap(arr.idx_name).unzip._1.reduce(_ || _)
        when (writeEn) {
          arr.vec(addrMux.toUInt) := nextMux
        }
      }
    })
    mod
  }
}
