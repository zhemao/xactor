package Xactor

import Chisel._
import Chisel.AdvTester._

class GcdActor(NumWidth: Int) extends Actor {
  val a = InQueue(UInt(width = NumWidth))
  val b = InQueue(UInt(width = NumWidth))
  val res = OutQueue(UInt(width = NumWidth))

  val x = State(UInt(width = NumWidth))
  val y = State(UInt(width = NumWidth))
  val running = State(init = Bool(false))

  action(List(a, b)) {
    case xinit :: yinit :: Nil => guard (!running.value) {
      x := xinit
      y := yinit
      running := Bool(true)
    }
  }

  action () {
    guard (x.value < y.value) {
      y := y.value - x.value
    }
  }

  action () {
    guard (y.value < x.value) {
      x := x.value - y.value
    }
  }

  action (res) {
    guard (running.value && x.value === y.value) {
      running := Bool(false)
      x.value
    }
  }
}

class GcdSetup(NumWidth: Int) extends Module {
  val io = new Bundle {
    val a = Decoupled(UInt(width = NumWidth)).flip
    val b = Decoupled(UInt(width = NumWidth)).flip
    val res = Decoupled(UInt(width = NumWidth))
  }

  val actor = new GcdActor(NumWidth).toMod

  val aqueue = Queue(io.a, 1)
  aqueue <> actor.portMap("a").asInstanceOf[DecoupledIO[UInt]]

  val bqueue = Queue(io.b, 1)
  bqueue <> actor.portMap("b").asInstanceOf[DecoupledIO[UInt]]

  val resqueue = Queue(actor.portMap("res").asInstanceOf[DecoupledIO[UInt]], 1)
  resqueue <> io.res
}

class GcdTester(c: GcdSetup) extends AdvTester(c) {
  isTrace = true
  wire_poke(c.io.a.bits, 12)
  wire_poke(c.io.a.valid, 1)
  wire_poke(c.io.b.bits, 10)
  wire_poke(c.io.b.valid, 1)
  takestep()
  wire_poke(c.io.a.valid, 0)
  wire_poke(c.io.b.valid, 0)
  takestep()

  until (peek(c.io.res.valid) == 1, 10) {}

  expect(c.io.res.bits, 2)
}

object GcdMain {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new GcdSetup(8))) {
      c => new GcdTester(c)
    }
  }
}
