Type solver uses two projects `OCanren` and `noCanren` with `wildcard` extension.

First, you need to install OCaml via OPAM, 4.14.1.

To install the projects you need to pin they:

    opam pin https://github.com/Lozov-Petr/OCanren.git#wildcard-experiments --yes
    opam pin https://github.com/PLTools/noCanren.git#wildcard --yes

To build, execute or test the solver you can use standart `dune` commands or commands provided by makefile.