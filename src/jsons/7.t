  $ export NOBENCH=1
  $ ./run_json.exe 7.json -n 3 #-v
  Table size: 8
  Running generated query
  	     Processing: ? <-< ICollection (int ())
  Fatal error: exception CT_of_json.Name_not_found("int")
  Raised at CT_of_json.make_classtable.id_of_name in file "CT_of_json.ml", line 635, characters 33-38
  Called from CT_of_json.make_query.prepare_goal_attempt.on_typ in file "CT_of_json.ml", line 677, characters 16-26
  Called from CT_of_json.make_query.prepare_goal_attempt.on_arg in file "CT_of_json.ml", line 715, characters 29-35
  Called from OCanren.Std.list in file "OCanren/src/OCanren.ml", line 74, characters 25-30
  Called from CT_of_json.make_query.prepare_goal_attempt.on_typ in file "CT_of_json.ml", line 682, characters 48-56
  Called from CT_of_json.make_query.prepare_goal_attempt.goal.(fun) in file "CT_of_json.ml", line 743, characters 48-54
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from CT_of_json.make_query.prepare_goal_attempt.goal in file "CT_of_json.ml", line 737, characters 8-22
  Called from Dune__exe__Run_json.(fun) in file "jsons/run_json.ml", line 153, characters 9-13
  Called from OCanren__Stream.bind.(fun) in file "OCanren/src/core/Stream.ml", line 123, characters 46-53
  Called from OCanren__Stream.map.(fun) in file "OCanren/src/core/Stream.ml", line 148, characters 47-53
  Called from OCanren__Stream.msplit in file "OCanren/src/core/Stream.ml", line 134, characters 29-34
  Called from Dune__exe__Run_json.run_jtype.(fun) in file "jsons/run_json.ml", line 41, characters 25-26
  Called from Dune__exe__Run_json.run_jtype.loop in file "jsons/run_json.ml", line 47, characters 12-16
  Called from Dune__exe__Run_json in file "jsons/run_json.ml", line 147, characters 2-11
  [2]
