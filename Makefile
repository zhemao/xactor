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

TEST_FLAGS = --genHarness --compile --backend c --test --vcd --debug

test-actor: $(example_src) $(core_gen_dir)/timestamp
	sbt "test:run-main Xactor.MyActorMain $(TEST_FLAGS) --targetDir $(example_gen_dir)"

test-module: $(example_src) $(core_gen_dir)/timestamp
	sbt "test:run-main Xactor.MyModuleMain $(TEST_FLAGS) --targetDir $(example_gen_dir)"

test-gcd: $(example_src) $(core_gen_dir)/timestamp
	sbt "test:run-main Xactor.GcdMain $(TEST_FLAGS) --targetDir $(example_gen_dir)"

test-fork: $(example_src) $(core_gen_dir)/timestamp
	sbt "test:run-main Xactor.ForkMain $(TEST_FLAGS) --targetDir $(example_gen_dir)"

$(core_gen_dir)/timestamp: $(core_src)
	sbt "compile"
	date > "$(core_gen_dir)/timestamp"

clean:
	rm -rf build/* target/* *.dot *.ps
