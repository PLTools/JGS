  $ export NOBENCH=1
  $ timeout 15 run_json2 -ct ../../../jsons_real/0.json java.util.List.json -n 20
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.util.List ((E <: java.lang.Object ()))
    1)  java.util.List<[(? extends java.lang.Object)]>
    2)  java.util.LinkedList<[(? extends java.lang.Object)]>
    3)  sun.awt.util.IdentityLinkedList<[(? extends java.lang.Object)]>
    4)  kotlin.collections.builders.ListBuilder<[(? extends java.lang.Object)]>
    5)  com.google.common.collect.ImmutableList<[(? extends java.lang.Object)]>
    6)  java.util.ArrayList<[(? extends java.lang.Object)]>
    7)  java.util.Vector<[(? extends java.lang.Object)]>
    8)  kotlin.collections.AbstractMutableList<[(? extends java.lang.Object)]>
    9)  sun.awt.util.IdentityArrayList<[(? extends java.lang.Object)]>
   10)  java.util.ImmutableCollections$AbstractImmutableList<[(? extends java.lang.Object)]>
   11)  kotlinx.collections.immutable.adapters.ImmutableListAdapter<[(? extends java.lang.Object)]>
   12)  org.jooq.impl.QueryPartListView<[(? extends java.lang.Object)]>
   13)  kotlinx.collections.immutable.PersistentList$Builder<[(? extends java.lang.Object)]>
   14)  com.google.common.collect.Synchronized$SynchronizedList<[(? extends java.lang.Object)]>
   15)  com.google.common.collect.ForwardingList<[(? extends java.lang.Object)]>
   16)  com.sun.tools.javac.util.List<[(? extends java.lang.Object)]>
   17)  java.util.AbstractList<[(? extends java.lang.Object)]>
   18)  java.util.Collections$CheckedList<[(? extends java.lang.Object)]>
   19)  java.util.Collections$SynchronizedList<[(? extends java.lang.Object)]>
   20)  java.util.Collections$UnmodifiableList<[(? extends java.lang.Object)]>
