#=======================================================================
# UCB Chisel Flow: Makefile 
#-----------------------------------------------------------------------
#
# This makefile will generate verilog files or an emulator from chisel code

core_src := Makefile src/main/scala/*.scala
example_src := Makefile src/test/scala/*.scala
example_gen_dir    := build/examples/generated-src
core_gen_dir    := target

.PHONY: test clean

test: $(example_src) $(core_gen_dir)/timestamp
	sbt "test:run-main Xactor.MyActorMain --genHarness --compile --backend c --targetDir $(example_gen_dir)"
	sbt "test:run-main Xactor.MyModuleMain --genHarness --compile --backend c --targetDir $(example_gen_dir)"

$(core_gen_dir)/timestamp: $(core_src)
	sbt "compile"
	date > "$(core_gen_dir)/timestamp"

clean:
	rm -rf build/* target/*  



