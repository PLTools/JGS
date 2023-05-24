  $ ../jsons/run_json.exe -n 1 0.json
  Fatal error: exception Failure("Class 'java.util.logging.StreamHandler' was not yet declared")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from JGS2.SampleCT.make_class_fix.(fun) in file "JGS2.ml", line 195, characters 38-43
  Called from JGS2.SampleCT.add_class_fix in file "JGS2.ml", line 173, characters 12-13
  Called from CT_of_json.make_classtable.on_decl in file "CT_of_json.ml", line 287, characters 10-27
  Called from Stdlib__List.iter in file "list.ml", line 110, characters 12-15
  Called from CT_of_json.make_classtable in file "CT_of_json.ml", line 354, characters 8-10
  Called from CT_of_json.make_query in file "CT_of_json.ml", line 371, characters 40-55
  Called from Dune__exe__Run_json in file "jsons/run_json.ml", line 32, characters 10-33
  [2]
