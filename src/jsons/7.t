  $ export NOBENCH=1
  $ export OCAMLRUNPARAM='b=0'
Only two answers expected
  $ ./run_json.exe 7.json -n 2
  Table size: 9
  Running generated query
  	     Processing: ? <-< ICollection (int ())
    1)  ICollection<[int]>
    2)  java.util.List<[int]>
