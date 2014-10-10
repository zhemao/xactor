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

  action (a) {
    x => guard (x > UInt(10)) {
      c <== x
    }
  }

  action (a) {
    x => guard (x <= UInt(10)) {
      c <== x
    }
  }

  action (b) {
    x => guard (Bool(true)) {
      when (s.value < UInt(10)) {
        s := s.value + x
      } .otherwise {
        c <== s.value
      }
    }
  }
}
