  $ export NOBENCH=1
  $ timeout 15 run_json2 -ct ../../../jsons_real/0.json java.util.Set.json -n 21
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.util.Set ((E <: java.lang.Object ()))
    1)  java.util.Set<[(? extends java.lang.Object)]>
    2)  java.util.LinkedHashSet<[(? extends java.lang.Object)]>
    3)  kotlin.collections.builders.MapBuilderKeys<[(? extends java.lang.Object)]>
    4)  kotlin.collections.builders.SetBuilder<[(? extends java.lang.Object)]>
    5)  com.google.common.collect.Sets$FilteredSet<[(? extends java.lang.Object)]>
    6)  com.google.common.collect.ImmutableSet<[(? extends java.lang.Object)]>
    7)  java.util.Collections$SetFromMap<[(? extends java.lang.Object)]>
    8)  java.util.HashSet<[(? extends java.lang.Object)]>
    9)  kotlin.collections.AbstractMutableSet<[(? extends java.lang.Object)]>
   10)  java.util.ImmutableCollections$AbstractImmutableSet<[(? extends java.lang.Object)]>
   11)  kotlinx.collections.immutable.PersistentSet$Builder<[(? extends java.lang.Object)]>
   12)  com.google.common.collect.Synchronized$SynchronizedSet<[(? extends java.lang.Object)]>
   13)  com.google.common.collect.ForwardingSet<[(? extends java.lang.Object)]>
   14)  java.util.AbstractSet<[(? extends java.lang.Object)]>
   15)  java.util.Collections$CheckedSet<[(? extends java.lang.Object)]>
   16)  java.util.Collections$SynchronizedSet<[(? extends java.lang.Object)]>
   17)  java.util.Collections$UnmodifiableSet<[(? extends java.lang.Object)]>
   18)  java.util.SortedSet<[(? extends java.lang.Object)]>
   19)  javax.security.auth.Subject$SecureSet<[(? extends java.lang.Object)]>
   20)  kotlin.collections.AbstractSet<[(? extends java.lang.Object)]>
   21)  kotlinx.collections.immutable.ImmutableSet<[(? extends java.lang.Object)]>
