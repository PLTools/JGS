build:
	dune build

run:
	dune exec ./JGS_run.exe

test:
	dune test

auto-promote:
	dune test --auto-promote

clean:
	dune clean

.PHONY: deps
deps:
	opam install -y \
		benchmark ppx_yojson_conv yojson ocamlformat ocaml-lsp-server mtime.1.4.0 mtime.1.4.0


RUN_EXAMPLE = OCAMLRUNPARAM='s=500M,h=500M' dune exec jsons/run_json2.exe --profile=release -- \
              -n 1000 -silent -need-simplified -ct jsons_real/0.json

UP_DOWN_EXAMPLES = up1_down1_1 up1_down1_2

EXAMPLES = down1_1 down1_2 down1_3 down1_4 \
           down2_1 down2_2 \
           down3_1 down3_2 \
           up1_1 up1_2 up1_3 \
           up2_1 up2_2 \
           up3_1 \
           $(UP_DOWN_EXAMPLES)

define LOPSTR_EXAMPLES

example_$(1):
	$(RUN_EXAMPLE) only_type_queries/LOPSTR2023/$(1).json

example_no_spec_$(1):
	$(RUN_EXAMPLE) -no-table-spec only_type_queries/LOPSTR2023/$(1).json

example_no_dyn_closure_$(1):
	$(RUN_EXAMPLE) -no-dynamic-closure only_type_queries/LOPSTR2023/$(1).json

example_no_opts_$(1):
	$(RUN_EXAMPLE) -no-table-spec -no-dynamic-closure only_type_queries/LOPSTR2023/$(1).json

endef

$(foreach i,$(EXAMPLES),$(eval $(call LOPSTR_EXAMPLES,$(i))))

define LOPSTR_UP_DOWN_EXAMPLES

example_upper_first_$(1):
	$(RUN_EXAMPLE) -upper-bound-first only_type_queries/LOPSTR2023/$(1).json

example_upper_first_no_spec_$(1):
	$(RUN_EXAMPLE) -upper-bound-first -no-table-spec only_type_queries/LOPSTR2023/$(1).json

example_upper_first_no_dyn_closure_$(1):
	$(RUN_EXAMPLE) -upper-bound-first -no-dynamic-closure only_type_queries/LOPSTR2023/$(1).json

example_upper_first_no_opts_$(1):
	$(RUN_EXAMPLE) -upper-bound-first -no-table-spec -no-dynamic-closure only_type_queries/LOPSTR2023/$(1).json

endef

$(foreach i,$(UP_DOWN_EXAMPLES),$(eval $(call LOPSTR_UP_DOWN_EXAMPLES,$(i))))

# Preapare target 'bench_LOPSTR' for 7 LOPSTR benchmarks
.PHONY: bench_LOPSTR bench_LOPSTR_run
JGS_REPEAT ?= 100
EXT     :=   TESTI TESTII TESTIII TESTIV TESTV TESTVI TESTVII  TESTIX
CODE    := down1_1 down2_1 down2_2   up3_0 up1_1  up2_1    up3_1   up1_down1_1
# Getters
JOINED  := $(join $(addsuffix :,$(EXT)),$(CODE))
GET_EXT  = $(word 1,$(subst :, ,$1))
GET_CODE = $(word 2,$(subst :, ,$1))

define BUILD_RULES
.PHONY: LOPSTR_$1
LOPSTR_$1: bench_LOPSTR_prepare
	JGS_BENCH=$(JGS_REPEAT) OCAMLRUNPARAM='s=500M,h=500M' taskset -c 0 ../_build/default/src/jsons/run_json2.exe \
		-n 1000 -silent -need-simplified -ct jsons_real/0.json \
		-la-testname $(1) -la-file $(1).tex only_type_queries/LOPSTR2023/$(2).json
bench_LOPSTR_run: LOPSTR_$1
endef
$(foreach j,$(JOINED),$(eval $(call BUILD_RULES,$(call GET_EXT,$j),$(call GET_CODE,$j))))

LOPSTR_TESTVIII:
	JGS_BENCH=$(JGS_REPEAT) $(RUN_EXAMPLE) -la-testname TESTVIII -la-file TESTVIII.tex only_type_queries/LOPSTR2023/up1_down1_1.json -lobefore
bench_LOPSTR_run: LOPSTR_TESTVIII

bench_LOPSTR: bench_LOPSTR_prepare bench_LOPSTR_run
	cat $(addsuffix .tex,$(EXT) TESTVIII) > LOPSTR.tex
	$(RM) $(addsuffix .tex,$(EXT) TESTVIII)
.PHONY: bench_LOPSTR_prepare
bench_LOPSTR_prepare:
	sudo cpupower -c 0 frequency-set --governor performance
	dune b jsons/run_json2.exe --profile=release

