  $ ./functionalTests.exe
   1 Object[] < Object (true) : true
   2 Object[] < Cloneable (true) : true
   3 Object[] < Serializable (true) : true
   4 Object < Object[] (false): false
   5 Cloneable < Object[] (false): false
   6 Serializable < Object[] (false): false
   7 Object[][] < Serializable[] (true) : true
   8 B < A (true) : true
   9 C < A (true) : true
  10 C < IA (true) : true
  11 IB < IA (true) : true
  12 F<A, B> < E<D<B>, A> (true) : true
  13 Collection<A> < Collection< VB extends Object> : false
  Fatal error: exception Failure("*** should not happen ***")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from JGS.Verifier.(-<-).capture_conversion.(fun) in file "JGS.ml", line 98, characters 33-47
  Called from Stdlib__List.map in file "list.ml", line 92, characters 20-23
  Called from JGS.Verifier.(-<-).capture_conversion in file "JGS.ml", line 96, characters 8-16
  Called from JGS.Verifier.(-<-) in file "JGS.ml", line 135, characters 14-32
  Called from Dune__exe__FunctionalTests in file "tests/functionalTests.ml", line 104, characters 4-231
  [2]
