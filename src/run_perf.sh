#!/usr/bin/env bash

set -ex
sudo sh -c 'echo 0 > /proc/sys/kernel/kptr_restrict'
if [ $? -ne 0 ]; then
    exit 1
fi
set +ex

CTLFIFO=ctl_fd.fifo
if [ -e $CTLFIFO ]; then
    rm -f $CTLFIFO
fi
mkfifo $CTLFIFO
FIFO_PATH=$(realpath $CTLFIFO)

JSON=only_type_queries/Iterable_of_object.json
JSON=only_type_queries/single_queries/extends_comparable_type_variables/java.lang.Iterable.json

ARGS="-n 5 -ct jsons_real/0.json $JSON -silent -perffifo $FIFO_PATH"
export OCAMLRUNPARAM='s=1000M,h=1000M'
export OCAMLRUNPARAM='s=1000M'
PERF_FLAGS="--control=fifo:$CTLFIFO --call-graph=dwarf"
dune b jsons/run_json2.exe --profile=release && \
    perf record $PERF_FLAGS _build/default/jsons/run_json2.exe $ARGS