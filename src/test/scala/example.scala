package Xactor

import Chisel._
import scala.collection.mutable.HashMap

class MyActor extends Actor {
  val a = InQueue(UInt(width = 5))
  val b = InQueue(UInt(width = 5))
  val c = OutQueue(UInt(width = 5))
  val d = InQueue(UInt(width = 5))
  val e = InQueue(UInt(width = 5))
  val f = InQueue(UInt(width = 1))
  val g = InQueue(UInt(width = 8))

  val s = State(init = UInt(0, 7))
  val arr = StateArray.fill(2){ State(init = UInt(0, 8)) }

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

  action(List(d, e), c) {
    case (x :: y :: Nil) => guard (x < y) {
      x
    }
  }

  action(List(d, e), c) {
    case (x :: y :: Nil) => guard (y <= x) {
      arr(0) := x + y
      arr(1) := arr(0).value
      arr(1).value
    }
  }

  action(List(f, g)) {
    case (idx :: data :: Nil) =>
      arr.write(idx, data)
  }

  action(f, c) {
    i => arr.read(i)
  }
}

class MyActorSetup extends Module {
  val io = new Bundle {
    val a = Decoupled(UInt(width = 5)).flip
    val b = Decoupled(UInt(width = 5)).flip
    val c = Decoupled(UInt(width = 5))
    val d = Decoupled(UInt(width = 5)).flip
    val e = Decoupled(UInt(width = 5)).flip
    val f = Decoupled(UInt(width = 1)).flip
    val g = Decoupled(UInt(width = 8)).flip
  }

  val actor = (new MyActor).toMod

  actor.connect("a", io.a)
  actor.connect("b", io.b)
  actor.connect("c", io.c)
  actor.connect("d", io.d)
  actor.connect("e", io.e)
  actor.connect("f", io.f)
  actor.connect("g", io.g)
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

  poke(c.io.d.valid, 1)
  poke(c.io.e.valid, 1)
  poke(c.io.d.bits, 4)
  poke(c.io.e.bits, 5)
  step(1)
  poke(c.io.d.valid, 0)
  poke(c.io.e.valid, 0)
  step(1)
  expect(c.io.c.valid, 1)
  expect(c.io.c.bits, 4)
  poke(c.io.c.ready, 1)
  step(1)
  poke(c.io.c.ready, 0)

  poke(c.io.d.valid, 1)
  poke(c.io.e.valid, 1)
  poke(c.io.d.bits, 4)
  poke(c.io.e.bits, 4)
  step(1)
  poke(c.io.d.valid, 0)
  poke(c.io.e.valid, 0)
  step(1)
  expect(c.io.c.valid, 1)
  expect(c.io.c.bits, 0)
  poke(c.io.c.ready, 1)
  step(1)
  poke(c.io.c.ready, 0)

  poke(c.io.f.valid, 1)
  poke(c.io.f.bits, 0)
  step(1)
  poke(c.io.f.valid, 0)
  step(1)
  expect(c.io.c.valid, 1)
  expect(c.io.c.bits, 8)
  poke(c.io.c.ready, 1)
  step(1)
  poke(c.io.c.ready, 0)

  poke(c.io.d.valid, 1)
  poke(c.io.e.valid, 1)
  poke(c.io.d.bits, 1)
  poke(c.io.e.bits, 0)
  step(1)
  poke(c.io.d.valid, 0)
  poke(c.io.e.valid, 0)
  step(1)
  expect(c.io.c.valid, 1)
  expect(c.io.c.bits, 0)
  poke(c.io.c.ready, 1)
  step(1)
  poke(c.io.c.ready, 0)

  poke(c.io.f.valid, 1)
  poke(c.io.f.bits, 1)
  step(1)
  poke(c.io.f.valid, 0)
  step(1)
  expect(c.io.c.valid, 1)
  expect(c.io.c.bits, 8)
  poke(c.io.c.ready, 1)
  step(1)
  poke(c.io.c.ready, 0)

  poke(c.io.d.valid, 1)
  poke(c.io.e.valid, 1)
  poke(c.io.d.bits, 1)
  poke(c.io.e.bits, 0)
  step(1)
  poke(c.io.d.valid, 0)
  poke(c.io.e.valid, 0)
  step(1)
  expect(c.io.c.valid, 1)
  expect(c.io.c.bits, 8)
  poke(c.io.c.ready, 1)
  step(1)
  poke(c.io.c.ready, 0)

  poke(c.io.f.valid, 1)
  poke(c.io.f.bits, 0)
  poke(c.io.g.valid, 1)
  poke(c.io.g.bits, 3)
  step(1)
  poke(c.io.f.valid, 0)
  poke(c.io.g.valid, 0)
  step(1)

  poke(c.io.f.valid, 1)
  poke(c.io.f.bits, 0)
  step(1)
  poke(c.io.f.valid, 0)
  step(1)
  expect(c.io.c.valid, 1)
  expect(c.io.c.bits, 3)
  poke(c.io.c.ready, 1)
  step(1)
  poke(c.io.c.ready, 0)
}

object MyActorMain {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new MyActorSetup)) {
      c => new MyActorTest(c)
    }
  }
}
