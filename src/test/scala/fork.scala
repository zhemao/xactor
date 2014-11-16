package Xactor

import Chisel._

class Fork(w: Int) extends Actor {
  val a = InQueue(UInt(width = w))
  val b = OutQueue(UInt(width = w))
  val c = OutQueue(UInt(width = w))
  val d = InQueue(UInt(width = w))

  action(List(a), List(b, c)) {
    case x :: Nil => List(x, x)
  }

  action(d, c) {
    x => x + UInt(1)
  }
}

class ForkSetup(w: Int) extends Module {
  val io = new Bundle {
    val a = Decoupled(UInt(width = w)).flip
    val b = Decoupled(UInt(width = w))
    val c = Decoupled(UInt(width = w))
    val d = Decoupled(UInt(width = w)).flip
  }

  val actor = new Fork(w).toMod
  actor.connect("a", io.a)
  actor.connect("b", io.b)
  actor.connect("c", io.c)
  actor.connect("d", io.d)
}

class ForkTest(c: ForkSetup) extends Tester(c) {
  poke(c.io.a.bits, 3)
  poke(c.io.a.valid, 1)
  poke(c.io.d.bits, 4)
  poke(c.io.d.valid, 1)
  step(1)
  poke(c.io.a.valid, 0)
  poke(c.io.d.valid, 0)
  step(1)
  expect(c.io.b.valid, 1)
  expect(c.io.c.valid, 1)
  expect(c.io.b.bits, 3)
  expect(c.io.c.bits, 3)
  poke(c.io.c.ready, 1)
  step(1)
  poke(c.io.c.ready, 0)

  poke(c.io.d.bits, 4)
  poke(c.io.d.valid, 1)
  step(1)
  poke(c.io.d.valid, 0)
  step(1)
  expect(c.io.c.valid, 1)
  expect(c.io.c.bits, 5)
  poke(c.io.c.ready, 1)
  step(1)
  poke(c.io.c.ready, 0)
}

object ForkMain {
  def main(args: Array[String]) {
    chiselMain(args, () => Module(new ForkSetup(8)),
      (c: ForkSetup) => new ForkTest(c))
  }
}
