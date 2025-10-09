dune exec jsons/run_json2.exe -- -n 5 -ct jsons_real/0.json only_type_queries/Iterable_of_object.json
  $ export NOBENCH=1
  $ timeout 15 run_json2 -ct ../jsons_real/0.json Iterable_of_object.json -n 5
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.lang.Iterable ((T <: java.lang.Object ()))
    1)  java.lang.Iterable<[(? extends java.lang.Object)]>
    2)  org.jooq.ResultQuery<[(? extends java.lang.Object)]>
    3)  org.jooq.Cursor<[(? extends java.lang.Object)]>
    4)  org.jooq.impl.AbstractCursor<[(? extends java.lang.Object)]>
    5)  java.nio.file.DirectoryStream<[(? extends java.lang.Object)]>
