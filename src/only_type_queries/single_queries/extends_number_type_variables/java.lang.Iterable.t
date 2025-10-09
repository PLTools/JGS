dune exec jsons/run_json2.exe -- -n 5 -ct jsons_real/0.json only_type_queries/single_queries/extends_number_type_variables/java.lang.Iterable.json
  $ export NOBENCH=1
  $ timeout 15 run_json2 -ct ../../../jsons_real/0.json java.lang.Iterable.json -n 5
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.lang.Iterable (? extends java.lang.Number ())
    1)  java.lang.Iterable<[java.lang.Number]>
    2)  org.jooq.ResultQuery<[java.lang.Number]>
    3)  org.jooq.Cursor<[java.lang.Number]>
    4)  org.jooq.impl.AbstractCursor<[java.lang.Number]>
    5)  java.nio.file.DirectoryStream<[java.lang.Number]>
