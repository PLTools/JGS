  $ export NOBENCH=1
  $ timeout 15 run_json2 -ct ../../../jsons_real/0.json java.util.Collection.json -n 16
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.util.Collection ((E <: java.lang.Object ()))
    1)  java.util.Collection<[(? extends java.lang.Object)]>
    2)  kotlin.collections.builders.MapBuilderValues<[(? extends java.lang.Object)]>
    3)  kotlin.collections.AbstractMutableCollection<[(? extends java.lang.Object)]>
    4)  kotlinx.collections.immutable.adapters.ImmutableCollectionAdapter<[(? extends java.lang.Object)]>
    5)  org.jooq.impl.QueryPartCollectionView<[(? extends java.lang.Object)]>
    6)  kotlinx.collections.immutable.PersistentCollection$Builder<[(? extends java.lang.Object)]>
    7)  com.google.common.collect.Synchronized$SynchronizedCollection<[(? extends java.lang.Object)]>
    8)  com.google.common.collect.ForwardingCollection<[(? extends java.lang.Object)]>
    9)  com.google.common.collect.Multiset<[(? extends java.lang.Object)]>
   10)  java.util.AbstractCollection<[(? extends java.lang.Object)]>
   11)  java.util.Collections$CheckedCollection<[(? extends java.lang.Object)]>
   12)  java.util.Collections$SynchronizedCollection<[(? extends java.lang.Object)]>
   13)  java.util.Collections$UnmodifiableCollection<[(? extends java.lang.Object)]>
   14)  java.util.List<[(? extends java.lang.Object)]>
   15)  java.util.Queue<[(? extends java.lang.Object)]>
   16)  java.util.Set<[(? extends java.lang.Object)]>
