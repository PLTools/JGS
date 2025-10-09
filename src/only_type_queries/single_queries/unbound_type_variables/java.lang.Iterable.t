  $ export NOBENCH=1
  $ timeout 15 run_json2 -ct ../../../jsons_real/0.json java.lang.Iterable.json -n 8
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.lang.Iterable ((T <: java.lang.Object ()))
    1)  java.lang.Iterable<[(? extends java.lang.Object)]>
    2)  org.jooq.ResultQuery<[(? extends java.lang.Object)]>
    3)  org.jooq.Cursor<[(? extends java.lang.Object)]>
    4)  org.jooq.impl.AbstractCursor<[(? extends java.lang.Object)]>
    5)  java.nio.file.DirectoryStream<[(? extends java.lang.Object)]>
    6)  com.google.common.collect.FluentIterable<[(? extends java.lang.Object)]>
    7)  com.google.common.collect.SortedIterable<[(? extends java.lang.Object)]>
    8)  com.google.common.graph.EndpointPair<[(? extends java.lang.Object)]>
