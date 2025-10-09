  $ export NOBENCH=1
For manual run: `dune exec jsons/run_json.exe -- -n 2 jsons_real/4.json`
# Asking for 1 answer hangs
  $ ../jsons/run_json.exe -n 0 4.json
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
