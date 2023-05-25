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
	opam install benchmark ppx_yojson_conv yojson ocamlformat ocaml-lsp-server mtime.1.4.0 -y