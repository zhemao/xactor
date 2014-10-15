package Xactor

import Chisel._

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
    x => guard (x > UInt(10, 5)) {
      x
    }
  }

  action (a, c) {
    x => guard (x <= UInt(10, 5)) {
      x + UInt(10, 5)
    }
  }

  action (b) {
    x => guard (Bool(true)) {
      when (s.value < UInt(10, 7)) {
        s := s.value + x
      } .otherwise {
        s := s.value + x - UInt(10, 7)
      }
    }
  }
}

object MyActorMain {
  def main(args: Array[String]) {
    chiselMain(args, () => (new MyActor).toMod)
  }
}
