Type solver uses two projects `OCanren` and `noCanren` with `wildcard` extension.

First, you need to install OCaml via OPAM, 4.14.1.

~~To install the projects you need to pin them~~ You don't, I have added submodules:

    opam pin https://github.com/Lozov-Petr/OCanren.git#wildcard-experiments --yes
    opam pin https://github.com/PLTools/noCanren.git#wildcard --yes

To build, execute or test the solver you can use standard `dune` commands or commands provided by makefile.

For example, run `dune b @jsons_real/guava/0` to test Guava table 0


#### Dependencies

    opam install OCanren OCanren-ppx --deps-only -y
    opam install ppx_yojson_conv -y
    make deps
