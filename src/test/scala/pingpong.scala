package Xactor

import Chisel._

class PingPong extends Actor {
  val ping = InQueue(UInt(width = 8))
  val pong = OutQueue(UInt(width = 8))

  val serve = InQueue(UInt(width = 8))
  val escape = OutQueue(UInt(width = 8))

  action(ping, escape) { x => x }
  action(serve, pong) { x => x }
  action(ping, pong) { x => x + UInt(1) }
}

class PingPongSetup extends Module {
  val io = new Bundle {
    val serve = Decoupled(UInt(width = 8)).flip
    val escape = Decoupled(UInt(width = 8))
  }

  val playera = new PingPong().toMod
  val playerb = new PingPong().toMod

  playera.connect(playerb, "ping", "pong")
  playera.connect(playerb, "pong", "ping")
  playera.connect("serve", io.serve)
  playera.connect("escape", io.escape)
}

class PingPongTest(c: PingPongSetup) extends Tester(c) {
  poke(c.io.serve.valid, 1)
  poke(c.io.serve.bits, 0)
  step(1)
  poke(c.io.serve.valid, 0)
  step(3)
  expect(c.io.escape.valid, 1)
  expect(c.io.escape.bits, 1)
}

object PingPongMain {
  def main(args: Array[String]) {
    chiselMain(args, () => Module(new PingPongSetup),
      (c: PingPongSetup) => new PingPongTest(c))
  }
}
