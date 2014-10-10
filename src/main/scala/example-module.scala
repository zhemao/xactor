package Xactor

import Chisel._

class MyModule extends Module {
  val io = new Bundle {
    val a = new DecoupledIO(UInt(width = 5)).flip
    val b = new DecoupledIO(UInt(width = 5)).flip
    val c = new DecoupledIO(UInt(width = 5))
  }

  val flags = Vec.fill(4){ Bool() }
  val g2_subflags = Vec.fill(2) { Bool() }

  val s = Reg(init = UInt(0, 7))

  flags(0) := io.a.valid && io.a.bits > UInt(10) && io.c.ready
  flags(1) := io.a.valid && io.a.bits <= UInt(10) && io.c.ready
  flags(2) := io.b.valid && Bool(true) && io.c.ready
  flags(3) := Bool(true)

  val c_outputs = Vec.fill(5){ UInt(width = 5) }

  c_outputs(0) := io.a.bits
  c_outputs(1) := io.a.bits
  c_outputs(2) := UInt(0)
  c_outputs(3) := s
  c_outputs(4) := UInt(0)

  val s_updates = Vec.fill(5) { UInt(width = 7) }
  s_updates(0) := s
  s_updates(1) := s
  s_updates(2) := s + io.a.bits
  s_updates(3) := s
  s_updates(4) := s

  val mux_sel = Vec.fill(5) { Bool() }

  mux_sel(0) := flags(0)
  mux_sel(1) := flags(1)
  mux_sel(2) := flags(2) & g2_subflags(0)
  mux_sel(3) := flags(2) & g2_subflags(1)
  mux_sel(4) := flags(3)

  io.c.bits := PriorityMux(mux_sel, c_outputs)
  s := PriorityMux(mux_sel, s_updates)

  io.a.ready := flags(0) | flags(1)
  io.b.ready := flags(2)

  io.c.valid := mux_sel(0) | mux_sel(1) | mux_sel(3)
}

class MyModuleTester(c: MyModule) extends Tester(c) {
  poke(c.io.a.bits, 16)
  poke(c.io.a.valid, 1)
  poke(c.io.b.valid, 1)
  poke(c.io.c.ready, 1)
  step(1)

  expect(c.io.c.valid, 1)
  expect(c.io.c.bits, 16)
}

object MyModuleMain {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new MyModule)) {
      c => new MyModuleTester(c)
    }
  }
}
