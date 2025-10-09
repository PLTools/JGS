  $ export NOBENCH=1
  $ timeout 15 run_json2 -ct ../../../jsons_real/0.json java.util.SortedSet.json -n 15
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.util.SortedSet ((E <: java.lang.Object ()))
    1)  java.util.SortedSet<[(? extends java.lang.Object)]>
    2)  com.google.common.collect.SortedMultisets$ElementSet<[(? extends java.lang.Object)]>
    3)  com.google.common.collect.Sets$FilteredSortedSet<[(? extends java.lang.Object)]>
    4)  com.google.common.collect.Synchronized$SynchronizedSortedSet<[(? extends java.lang.Object)]>
    5)  com.google.common.collect.ForwardingSortedSet<[(? extends java.lang.Object)]>
    6)  java.util.Collections$CheckedSortedSet<[(? extends java.lang.Object)]>
    7)  java.util.Collections$SynchronizedSortedSet<[(? extends java.lang.Object)]>
    8)  java.util.Collections$UnmodifiableSortedSet<[(? extends java.lang.Object)]>
    9)  java.util.NavigableSet<[(? extends java.lang.Object)]>
   10)  com.google.common.collect.SortedMultisets$NavigableElementSet<[(? extends java.lang.Object)]>
   11)  com.google.common.collect.Sets$FilteredNavigableSet<[(? extends java.lang.Object)]>
   12)  com.google.common.collect.Maps$SortedKeySet<[(? extends java.lang.Object); Not implemented JGS_Helpers.ml 74]>
   13)  com.google.common.collect.Synchronized$SynchronizedNavigableSet<[(? extends java.lang.Object)]>
  ERROR: free variables inside class id
   14)  com.google.common.collect.Maps$SortedKeySet<[(? extends java.lang.Object); _.??]>
   15)  com.google.common.collect.Maps$SortedKeySet<[(? extends java.lang.Object); (? extends java.lang.Object super Array<java.lang.Object>)]>
