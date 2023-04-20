  $ ./JGS_run.exe
  Fuctional tests:
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
  
  
  ****************************************************************************************************
  
  
  
  Relational tests (forward):
   1 Object[] < Object (true) : [true]
   2 Object[] < Cloneable (true) : [true]
   3 Object[] < Serializable (true) : [true]
   4 Object < Object[] (false): [false]
   5 Cloneable < Object[] (false):[false]
   6 Serializable < Object[] (false): [false]
   7 Object[][] < Serializable[] (true) : [true]
   8 B < A (true) : [true]
   9 C < A (true) : [true]
  10 C < IA (true) : [true]
  11 IB < IA (true) : [true]
  12 F<A, B> < E<D<B>, A> (true) : [true]
  
  
  ****************************************************************************************************
  
  
  
  Relational tests (backward):
  1.1 (?) < Object : 
  [
    Class (1, []);
    Var {id=_.38, index=_.39, upb=Class (1, []), lwb=_.41};
    Null;
    Intersect ([Class (1, []) | _.96]);
    Array (Class (1, []));
    Intersect ([_.98 [=/= Class (1, [])]; Class (1, []) | _.156]);
    Intersect ([_.98 [=/= Class (1, [])]; _.158 [=/= Class (1, [])]; Class (1, []) | _.183]);
    Intersect ([_.98 [=/= Class (1, [])]; _.158 [=/= Class (1, [])]; _.185 [=/= Class (1, [])]; Class (1, []) | _.209]);
    Intersect ([_.98 [=/= Class (1, [])]; _.158 [=/= Class (1, [])]; _.185 [=/= Class (1, [])]; _.211 [=/= Class (1, [])]; Class (1, []) | _.233]);
    Intersect ([_.98 [=/= Class (1, [])]; _.158 [=/= Class (1, [])]; _.185 [=/= Class (1, [])]; _.211 [=/= Class (1, [])]; _.235 [=/= Class (1, [])]; Class (1, []) | _.251])
  ]
  
  
  ****************************************************************************************************
  
  1.2 Object[] < (?) : 
  [
    Class (1, []);
    Interface (2, []);
    Interface (3, []);
    Array (Var {id=_.73, index=_.74, upb=_.75, lwb=Some (Class (1, []))});
    Array (Interface (1, []));
    Array (Class (1, []))
  ]
  
  
  ****************************************************************************************************
  
  2 (?) < Cloneable : 
  [
    Class (2, []);
    Var {id=_.38, index=_.39, upb=Interface (2, []), lwb=_.41};
    Null;
    Intersect ([Interface (2, []) | _.96]);
    Array (Class (1, []));
    Intersect ([_.98 [=/= Interface (2, [])]; Interface (2, []) | _.155]);
    Intersect ([_.98 [=/= Interface (2, [])]; _.157 [=/= Interface (2, [])]; Interface (2, []) | _.191]);
    Intersect ([_.98 [=/= Interface (2, [])]; _.157 [=/= Interface (2, [])]; _.193 [=/= Interface (2, [])]; Interface (2, []) | _.220]);
    Intersect ([_.98 [=/= Interface (2, [])]; _.157 [=/= Interface (2, [])]; _.193 [=/= Interface (2, [])]; _.222 [=/= Interface (2, [])]; Interface (2, []) | _.241]);
    Intersect ([_.98 [=/= Interface (2, [])]; _.157 [=/= Interface (2, [])]; _.193 [=/= Interface (2, [])]; _.222 [=/= Interface (2, [])]; _.243 [=/= Interface (2, [])]; Interface (2, []) | _.264])
  ]
  
  
  ****************************************************************************************************
  
  3 (?) < Serializable : 
  [
    Class (3, []);
    Var {id=_.38, index=_.39, upb=Interface (3, []), lwb=_.41};
    Null;
    Intersect ([Interface (3, []) | _.96]);
    Array (Class (1, []));
    Intersect ([_.98 [=/= Interface (3, [])]; Interface (3, []) | _.157]);
    Intersect ([_.98 [=/= Interface (3, [])]; _.159 [=/= Interface (3, [])]; Interface (3, []) | _.201]);
    Intersect ([_.98 [=/= Interface (3, [])]; _.159 [=/= Interface (3, [])]; _.203 [=/= Interface (3, [])]; Interface (3, []) | _.221]);
    Intersect ([_.98 [=/= Interface (3, [])]; _.159 [=/= Interface (3, [])]; _.203 [=/= Interface (3, [])]; _.223 [=/= Interface (3, [])]; Interface (3, []) | _.242]);
    Intersect ([_.98 [=/= Interface (3, [])]; _.159 [=/= Interface (3, [])]; _.203 [=/= Interface (3, [])]; _.223 [=/= Interface (3, [])]; _.244 [=/= Interface (3, [])]; Interface (3, []) | _.266])
  ]
  
  
  ****************************************************************************************************
  
  4.1 (?) < Object[] : 
  [
    Var {id=_.36, index=_.37, upb=Array (Class (1, [])), lwb=_.39};
    Null;
    Intersect ([Array (Class (1, [])) | _.63]);
    Intersect ([_.65 [=/= Array (Class (1, []))]; Array (Class (1, [])) | _.78]);
    Intersect ([_.65 [=/= Array (Class (1, []))]; _.80 [=/= Array (Class (1, []))]; Array (Class (1, [])) | _.92]);
    Intersect ([_.65 [=/= Array (Class (1, []))]; _.80 [=/= Array (Class (1, []))]; _.94 [=/= Array (Class (1, []))]; Array (Class (1, [])) | _.107]);
    Intersect ([_.65 [=/= Array (Class (1, []))]; _.80 [=/= Array (Class (1, []))]; _.94 [=/= Array (Class (1, []))]; _.109 [=/= Array (Class (1, []))]; Array (Class (1, [])) | _.125]);
    Intersect ([_.65 [=/= Array (Class (1, []))]; _.80 [=/= Array (Class (1, []))]; _.94 [=/= Array (Class (1, []))]; _.109 [=/= Array (Class (1, []))]; _.127 [=/= Array (Class (1, []))]; Array (Class (1, [])) | _.141]);
    Intersect ([_.65 [=/= Array (Class (1, []))]; _.80 [=/= Array (Class (1, []))]; _.94 [=/= Array (Class (1, []))]; _.109 [=/= Array (Class (1, []))]; _.127 [=/= Array (Class (1, []))]; _.143 [=/= Array (Class (1, []))]; Array (Class (1, [])) | _.164]);
    Intersect ([_.65 [=/= Array (Class (1, []))]; _.80 [=/= Array (Class (1, []))]; _.94 [=/= Array (Class (1, []))]; _.109 [=/= Array (Class (1, []))]; _.127 [=/= Array (Class (1, []))]; _.143 [=/= Array (Class (1, []))]; _.166 [=/= Array (Class (1, []))]; Array (Class (1, [])) | _.189])
  ]
  
  
  ****************************************************************************************************
  
  4.2 Object < (?) : 
  [
    Var {id=_.32, index=_.33, upb=_.34, lwb=Some (Class (1, []))};
    Interface (1, []);
    Class (1, [])
  ]
  
  
  ****************************************************************************************************
  
  5 Cloneable < (?): 
  [
    Var {id=_.34, index=_.35, upb=_.36, lwb=Some (Interface (2, []))}
  ]
  
  
  ****************************************************************************************************
  
  6 Serializable < (?) : 
  [
    Var {id=_.34, index=_.35, upb=_.36, lwb=Some (Interface (3, []))}
  ]
  
  
  ****************************************************************************************************
  
  7.1 (?) < Serializable[] : 
  [
    Var {id=_.36, index=_.37, upb=Array (Interface (3, [])), lwb=_.39};
    Null;
    Intersect ([Array (Interface (3, [])) | _.63]);
    Intersect ([_.65 [=/= Array (Interface (3, []))]; Array (Interface (3, [])) | _.78]);
    Intersect ([_.65 [=/= Array (Interface (3, []))]; _.80 [=/= Array (Interface (3, []))]; Array (Interface (3, [])) | _.92]);
    Intersect ([_.65 [=/= Array (Interface (3, []))]; _.80 [=/= Array (Interface (3, []))]; _.94 [=/= Array (Interface (3, []))]; Array (Interface (3, [])) | _.107]);
    Intersect ([_.65 [=/= Array (Interface (3, []))]; _.80 [=/= Array (Interface (3, []))]; _.94 [=/= Array (Interface (3, []))]; _.109 [=/= Array (Interface (3, []))]; Array (Interface (3, [])) | _.125]);
    Intersect ([_.65 [=/= Array (Interface (3, []))]; _.80 [=/= Array (Interface (3, []))]; _.94 [=/= Array (Interface (3, []))]; _.109 [=/= Array (Interface (3, []))]; _.127 [=/= Array (Interface (3, []))]; Array (Interface (3, [])) | _.141]);
    Intersect ([_.65 [=/= Array (Interface (3, []))]; _.80 [=/= Array (Interface (3, []))]; _.94 [=/= Array (Interface (3, []))]; _.109 [=/= Array (Interface (3, []))]; _.127 [=/= Array (Interface (3, []))]; _.143 [=/= Array (Interface (3, []))]; Array (Interface (3, [])) | _.164]);
    Intersect ([_.65 [=/= Array (Interface (3, []))]; _.80 [=/= Array (Interface (3, []))]; _.94 [=/= Array (Interface (3, []))]; _.109 [=/= Array (Interface (3, []))]; _.127 [=/= Array (Interface (3, []))]; _.143 [=/= Array (Interface (3, []))]; _.166 [=/= Array (Interface (3, []))]; Array (Interface (3, [])) | _.189])
  ]
  
  
  ****************************************************************************************************
  
  7.2 Object[][] < (?) : 
  [
    Array (Class (1, []));
    Array (Interface (2, []));
    Array (Interface (3, []));
    Array (Array (Var {id=_.100, index=_.101, upb=_.102, lwb=Some (Class (1, []))}));
    Array (Array (Interface (1, [])));
    Array (Array (Class (1, [])))
  ]
  
  
  ****************************************************************************************************
  
  Class A: 13
  
  Class B: 14
  
  8.1 (?) < A : 
  [
    Class (13, []);
    Var {id=_.38, index=_.39, upb=Class (13, []), lwb=_.41};
    Null;
    Intersect ([Class (13, []) | _.94]);
    Intersect ([_.96 [=/= Class (13, [])]; Class (13, []) | _.143]);
    Class (14, _.15);
    Intersect ([_.96 [=/= Class (13, [])]; _.145 [=/= Class (13, [])]; Class (13, []) | _.179]);
    Intersect ([_.96 [=/= Class (13, [])]; _.145 [=/= Class (13, [])]; _.181 [=/= Class (13, [])]; Class (13, []) | _.198]);
    Intersect ([_.96 [=/= Class (13, [])]; _.145 [=/= Class (13, [])]; _.181 [=/= Class (13, [])]; _.200 [=/= Class (13, [])]; Class (13, []) | _.218]);
    Intersect ([_.96 [=/= Class (13, [])]; _.145 [=/= Class (13, [])]; _.181 [=/= Class (13, [])]; _.200 [=/= Class (13, [])]; _.220 [=/= Class (13, [])]; Class (13, []) | _.247])
  ]
  
  
  ****************************************************************************************************
  
  8.2 B < (?) : 
  [
    Var {id=_.32, index=_.33, upb=_.34, lwb=Some (Class (14, []))};
    Interface (14, []);
    Class (14, []);
    Interface (13, []);
    Class (13, [])
  ]
  
  
  ****************************************************************************************************
  
  8.3 (?) < B : 
  [
    Class (14, []);
    Var {id=_.38, index=_.39, upb=Class (14, []), lwb=_.41};
    Null;
    Intersect ([Class (14, []) | _.94]);
    Intersect ([_.96 [=/= Class (14, [])]; Class (14, []) | _.144]);
    Intersect ([_.96 [=/= Class (14, [])]; _.146 [=/= Class (14, [])]; Class (14, []) | _.178]);
    Intersect ([_.96 [=/= Class (14, [])]; _.146 [=/= Class (14, [])]; _.180 [=/= Class (14, [])]; Class (14, []) | _.199]);
    Intersect ([_.96 [=/= Class (14, [])]; _.146 [=/= Class (14, [])]; _.180 [=/= Class (14, [])]; _.201 [=/= Class (14, [])]; Class (14, []) | _.232]);
    Intersect ([_.96 [=/= Class (14, [])]; _.146 [=/= Class (14, [])]; _.180 [=/= Class (14, [])]; _.201 [=/= Class (14, [])]; _.234 [=/= Class (14, [])]; Class (14, []) | _.255]);
    Intersect ([_.96 [=/= Class (14, [])]; _.146 [=/= Class (14, [])]; _.180 [=/= Class (14, [])]; _.201 [=/= Class (14, [])]; _.234 [=/= Class (14, [])]; _.257 [=/= Class (14, [])]; Class (14, []) | _.275])
  ]
  
  
  ****************************************************************************************************
  
  8.4 A < (?) : 
  [
    Var {id=_.32, index=_.33, upb=_.34, lwb=Some (Class (13, []))};
    Interface (13, []);
    Class (13, []);
    Interface (1, []);
    Class (1, [])
  ]
  
  
  ****************************************************************************************************
  
  Interface A: 21
  
  Class C: 22
  
  9 C < (?) : 
  [
    Var {id=_.32, index=_.33, upb=_.34, lwb=Some (Class (22, []))};
    Interface (22, []);
    Class (22, []);
    Interface (13, []);
    Class (13, []);
    Interface (21, []);
    Class (21, [])
  ]
  
  
  ****************************************************************************************************
  
  10.1 (?) < IA : 
  [
    Class (21, []);
    Var {id=_.38, index=_.39, upb=Interface (21, []), lwb=_.41};
    Null;
    Intersect ([Interface (21, []) | _.94]);
    Intersect ([_.96 [=/= Interface (21, [])]; Interface (21, []) | _.159]);
    Intersect ([_.96 [=/= Interface (21, [])]; _.161 [=/= Interface (21, [])]; Interface (21, []) | _.190]);
    Intersect ([_.96 [=/= Interface (21, [])]; _.161 [=/= Interface (21, [])]; _.192 [=/= Interface (21, [])]; Interface (21, []) | _.206]);
    Intersect ([_.96 [=/= Interface (21, [])]; _.161 [=/= Interface (21, [])]; _.192 [=/= Interface (21, [])]; _.208 [=/= Interface (21, [])]; Interface (21, []) | _.227]);
    Intersect ([_.96 [=/= Interface (21, [])]; _.161 [=/= Interface (21, [])]; _.192 [=/= Interface (21, [])]; _.208 [=/= Interface (21, [])]; _.229 [=/= Interface (21, [])]; Interface (21, []) | _.247]);
    Intersect ([_.96 [=/= Interface (21, [])]; _.161 [=/= Interface (21, [])]; _.192 [=/= Interface (21, [])]; _.208 [=/= Interface (21, [])]; _.229 [=/= Interface (21, [])]; _.249 [=/= Interface (21, [])]; Interface (21, []) | _.271])
  ]
  
  
  ****************************************************************************************************
  
  10.2 C < (?) : 
  [
    Var {id=_.32, index=_.33, upb=_.34, lwb=Some (Class (22, []))};
    Interface (22, []);
    Class (22, []);
    Interface (13, []);
    Class (13, []);
    Interface (21, []);
    Class (21, [])
  ]
  
  
  ****************************************************************************************************
  
  Interface B: 26
  
  11 IB < (?) : 
  [
    Var {id=_.34, index=_.35, upb=_.36, lwb=Some (Interface (26, []))};
    Class (26, []);
    Interface (26, []);
    Class (21, []);
    Interface (21, [])
  ]
  
  
  ****************************************************************************************************
  
  Class D<X>: 27
  
  Class E<X, Y>: 28
  
  Class F<X, Y> : 31
  
  12.1 (?) < E<D<B>, A> : 
  [
    Var {id=_.38, index=_.39, upb=Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))]), lwb=_.41};
    Null;
    Intersect ([Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))]) | _.94]);
    Intersect ([_.96 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))]) | _.194]);
    Intersect ([_.96 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.196 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))]) | _.254]);
    Intersect ([_.96 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.196 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.256 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))]) | _.296]);
    Intersect ([_.96 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.196 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.256 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.298 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))]) | _.331]);
    Intersect ([_.96 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.196 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.256 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.298 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.333 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))]) | _.344]);
    Intersect ([_.96 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.196 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.256 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.298 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.333 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.346 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))]) | _.361]);
    Intersect ([_.96 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.196 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.256 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.298 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.333 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.346 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; _.363 [=/= Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])]; Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))]) | _.378])
  ]
  
  
  ****************************************************************************************************
  
  12.2 (? - is class) < E<D<B>, A> : 
  [
    Class (31, [Type (Class (13, [])); Type (Class (14, []))]);
    Class (31, [Type (Class (13, [])); Type (Class (14, [])); Type (Class (_.25021, []))]);
    Class (31, [Type (Class (13, [])); Type (Class (14, [])); Type (Null)]);
    Class (31, [Type (Class (13, [])); Type (Class (14, [])); Type (Interface (_.34361, []))]);
    Class (31, [Type (Class (13, [])); Type (Class (14, [])); Type (Array (Class (_.38761, [])))]);
    Class (31, [Type (Class (13, [])); Type (Class (14, [])); Type (Array (Null))]);
    Class (31, [Type (Class (13, [])); Type (Class (14, [])); Type (Intersect ([]))]);
    Class (31, [Type (Class (13, [])); Type (Class (14, [])); Type (Class (_.55805, [Wildcard (None)]))]);
    Class (31, [Type (Class (13, [])); Type (Class (14, [])); Type (Array (Interface (_.59043, [])))]);
    Class (31, [Type (Class (13, [])); Type (Class (14, [])); Type (Array (Array (Class (_.65364, []))))])
  ]
  
  
  ****************************************************************************************************
  
  12.3 F<A, B> < (?) : 
  [
    Var {id=_.32, index=_.33, upb=_.34, lwb=Some (Class (31, [Type (Class (13, [])); Type (Class (14, []))]))};
    Interface (31, [Type (Class (13, [])); Type (Class (14, []))]);
    Class (31, [Type (Class (13, [])); Type (Class (14, []))]);
    Interface (31, [Type (Class (13, [])); Wildcard (Some ((Extends, Class (14, []))))]);
    Class (31, [Type (Class (13, [])); Wildcard (Some ((Extends, Class (14, []))))]);
    Interface (31, [Type (Class (13, [])); Wildcard (Some ((Super, Class (14, []))))]);
    Class (31, [Type (Class (13, [])); Wildcard (Some ((Super, Class (14, []))))]);
    Interface (31, [Wildcard (Some ((Extends, Class (13, [])))); Type (Class (14, []))]);
    Class (31, [Wildcard (Some ((Extends, Class (13, [])))); Type (Class (14, []))]);
    Interface (31, [Wildcard (Some ((Extends, Class (13, [])))); Wildcard (Some ((Extends, Class (14, []))))]);
    Class (31, [Wildcard (Some ((Extends, Class (13, [])))); Wildcard (Some ((Extends, Class (14, []))))]);
    Interface (31, [Wildcard (Some ((Super, Class (13, [])))); Type (Class (14, []))]);
    Class (31, [Wildcard (Some ((Super, Class (13, [])))); Type (Class (14, []))]);
    Interface (31, [Wildcard (Some ((Extends, Class (13, [])))); Wildcard (Some ((Super, Class (14, []))))]);
    Class (31, [Wildcard (Some ((Extends, Class (13, [])))); Wildcard (Some ((Super, Class (14, []))))]);
    Interface (31, [Wildcard (Some ((Super, Class (13, [])))); Wildcard (Some ((Extends, Class (14, []))))]);
    Class (31, [Wildcard (Some ((Super, Class (13, [])))); Wildcard (Some ((Extends, Class (14, []))))]);
    Interface (31, [Wildcard (Some ((Super, Class (13, [])))); Wildcard (Some ((Super, Class (14, []))))]);
    Class (31, [Wildcard (Some ((Super, Class (13, [])))); Wildcard (Some ((Super, Class (14, []))))]);
    Interface (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))]);
    Class (28, [Type (Class (27, [Type (Class (14, []))])); Type (Class (13, []))])
  ]
