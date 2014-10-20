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

class MyActorTest(c: ActorModule) extends Tester(c) {
  poke(c.portMap("a").bits.asInstanceOf[Bits], 0)
  poke(c.portMap("a").valid, 1)
  poke(c.portMap("c").ready, 1)
  step(1)
  expect(c.portMap("c").valid, 1)
  expect(c.portMap("c").bits.asInstanceOf[Bits], 10)
  poke(c.portMap("a").valid, 0)
  poke(c.portMap("c").ready, 0)

  poke(c.portMap("b").bits.asInstanceOf[Bits], 20)
  poke(c.portMap("b").valid, 1)
  step(1)
  expect(c.portMap("c").valid, 0)

  poke(c.portMap("b").bits.asInstanceOf[Bits], 5)
  poke(c.portMap("c").ready, 1)
  step(1)
  expect(c.portMap("c").valid, 1)
  expect(c.portMap("c").bits.asInstanceOf[Bits], 20)
}

object MyActorMain {
  def main(args: Array[String]) {
    chiselMainTest(args, () => (new MyActor).toMod) {
      c => new MyActorTest(c)
    }
  }
}
