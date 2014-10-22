package Xactor

import Chisel._
import scala.collection.mutable.HashMap

class MyActor extends Actor {
  val a = InQueue(UInt(width = 5))
  val b = InQueue(UInt(width = 5))
  val c = OutQueue(UInt(width = 5))

  val s = State(init = UInt(0, 7))

  /*action(a, b) {
    (x, y) => guard (Bool(true)) {
    }
  }*/


  action (a, c) {
    x => guard (x > UInt(10)) {
      x
    }
  }

  action (a, c) {
    x => guard (x <= UInt(10)) {
      x + UInt(10)
    }
  }

  action (b) {
    x => guard (s.value < UInt(20)) {
      s := s.value + x
    }
  }
  action (b, c) {
    x => guard (s.value >= UInt(20)) {
      s := x
      s.value
    }
  }
}

class MyActorSetup extends Module {
  val io = new Bundle {
    val a = Decoupled(UInt(width = 5)).flip
    val b = Decoupled(UInt(width = 5)).flip
    val c = Decoupled(UInt(width = 5))
  }

  val actor = (new MyActor).toMod

  val aqueue = Queue(io.a, 1)
  aqueue <> actor.portMap("a").asInstanceOf[DecoupledIO[UInt]]

  val bqueue = Queue(io.b, 1)
  bqueue <> actor.portMap("b").asInstanceOf[DecoupledIO[UInt]]

  val cqueue = Queue(actor.portMap("c").asInstanceOf[DecoupledIO[UInt]], 1)
  cqueue <> io.c
}

class MyActorTest(c: MyActorSetup) extends Tester(c) {
  poke(c.io.a.bits, 1)
  poke(c.io.a.valid, 1)
  expect(c.io.a.ready, 1)
  step(1)
  poke(c.io.a.valid, 0)
  step(1)
  expect(c.io.c.valid, 1)
  expect(c.io.c.bits, 11)
  poke(c.io.c.ready, 1)
  step(1)
  poke(c.io.c.ready, 0)

  expect(c.io.b.ready, 1)
  poke(c.io.b.bits, 20)
  poke(c.io.b.valid, 1)
  step(1)
  poke(c.io.b.valid, 0)
  step(1)
  expect(c.io.c.valid, 0)

  expect(c.io.b.ready, 1)
  poke(c.io.b.bits, 5)
  poke(c.io.b.valid, 1)
  step(1)
  poke(c.io.b.valid, 0)
  step(1)
  expect(c.io.c.valid, 1)
  expect(c.io.c.bits, 20)
  poke(c.io.c.ready, 1)
  step(1)
  poke(c.io.c.ready, 0)

  poke(c.io.b.valid, 0)
  step(1)
  expect(c.io.c.valid, 0)
}

object MyActorMain {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new MyActorSetup)) {
      c => new MyActorTest(c)
    }
  }
}
