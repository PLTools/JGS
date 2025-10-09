  $ export NOBENCH=1
  $ timeout 15 run_json2 -ct ../../../jsons_real/0.json java.util.NavigableSet.json -n 20
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.util.NavigableSet ((E <: java.lang.Object ()))
    1)  java.util.NavigableSet<[(? extends java.lang.Object)]>
    2)  com.google.common.collect.ImmutableSortedSet<[(? extends java.lang.Object)]>
    3)  com.google.common.collect.SortedMultisets$NavigableElementSet<[(? extends java.lang.Object)]>
    4)  com.google.common.collect.Sets$FilteredNavigableSet<[(? extends java.lang.Object)]>
    5)  com.google.common.collect.Synchronized$SynchronizedNavigableSet<[(? extends java.lang.Object)]>
    6)  com.google.common.collect.ForwardingNavigableSet<[(? extends java.lang.Object)]>
    7)  com.google.common.collect.Sets$UnmodifiableNavigableSet<[(? extends java.lang.Object)]>
    8)  java.util.Collections$CheckedNavigableSet<[(? extends java.lang.Object)]>
    9)  java.util.Collections$SynchronizedNavigableSet<[(? extends java.lang.Object)]>
   10)  java.util.Collections$UnmodifiableNavigableSet<[(? extends java.lang.Object)]>
   11)  java.util.TreeMap$KeySet<[(? extends java.lang.Object)]>
   12)  java.util.TreeSet<[(? extends java.lang.Object)]>
   13)  java.util.concurrent.ConcurrentSkipListSet<[(? extends java.lang.Object)]>
   14)  com.google.common.collect.ContiguousSet<[(? extends java.lang.Object)]>
   15)  com.google.common.collect.DescendingImmutableSortedSet<[(? extends java.lang.Object)]>
   16)  com.google.common.collect.RegularImmutableSortedSet<[(? extends java.lang.Object)]>
   17)  com.google.common.collect.Maps$NavigableKeySet<[(? extends java.lang.Object); Not implemented JGS_Helpers.ml 74]>
   18)  java.util.concurrent.ConcurrentSkipListMap$KeySet<[(? extends java.lang.Object); Not implemented JGS_Helpers.ml 74]>
   19)  com.google.common.collect.EmptyContiguousSet<[(? extends java.lang.Object)]>
   20)  com.google.common.collect.RegularContiguousSet<[(? extends java.lang.Object)]>
