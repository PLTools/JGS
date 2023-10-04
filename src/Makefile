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




RUN_EXAMPLE = dune exec jsons/run_json2.exe --profile=release -- \
              -n 1000 -silent -ct jsons_real/0.json

UP_DOWN_EXAMPLES = up1_down1_1 up1_down1_2

EXAMPLES = down1_1 down1_2 down1_3 down1_4 \
           down2_1 down2_2 \
           down3_1 down3_2 \
           up1_1 up1_2 up1_3 \
           up2_1 up2_2 \
           up3_1 \
           $(UP_DOWN_EXAMPLES)

define LOPSTR_EXAMPLES

example_structural_$(1):
	$(RUN_EXAMPLE) -remove-dups-structural only_type_queries/LOPSTR2023/$(1).json

example_debug_var_$(1):
	$(RUN_EXAMPLE) -remove-dups-debug-var only_type_queries/LOPSTR2023/$(1).json

example_with_dups_$(1):
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


example_upper_first_structural_$(1):
	$(RUN_EXAMPLE) -upper-bound-first -remove-dups-structural only_type_queries/LOPSTR2023/$(1).json

example_upper_first_debug_var_$(1):
	$(RUN_EXAMPLE) -upper-bound-first -remove-dups-debug-var only_type_queries/LOPSTR2023/$(1).json

example_upper_first_with_dups_$(1):
	$(RUN_EXAMPLE) -upper-bound-first only_type_queries/LOPSTR2023/$(1).json

example_upper_first_no_spec_$(1):
	$(RUN_EXAMPLE) -upper-bound-first -no-table-spec only_type_queries/LOPSTR2023/$(1).json

example_upper_first_no_dyn_closure_$(1):
	$(RUN_EXAMPLE) -upper-bound-first -no-dynamic-closure only_type_queries/LOPSTR2023/$(1).json

example_upper_first_no_opts_$(1):
	$(RUN_EXAMPLE) -upper-bound-first -no-table-spec -no-dynamic-closure only_type_queries/LOPSTR2023/$(1).json

endef

$(foreach i,$(UP_DOWN_EXAMPLES),$(eval $(call LOPSTR_UP_DOWN_EXAMPLES,$(i))))