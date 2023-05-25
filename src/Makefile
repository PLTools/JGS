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