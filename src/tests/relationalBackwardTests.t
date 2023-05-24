  $ ./relationalBackwardTests.exe
  1.1 (?) < Object : 
  [
    Null;
    Class (1, []);
    Array (Class (1, []));
    Interface (2, _.17);
    Interface (3, _.17)
  ]
  
  
  ****************************************************************************************************
  
  1.2 Object[] < (?) : 
  [
    Class (1, []);
    Interface (2, []);
    Interface (3, []);
    Array (Var {id=_.73, index=_.74, upb=_.75, lwb=Some (Class (1, []))});
    Array (Interface (1, []));
    Array (Class (1, []));
    Array (Interface (0, []));
    Array (Class (0, []))
  ]
  
  
  ****************************************************************************************************
  
  2 (?) < Cloneable : 
  [
    Null;
    Class (2, []);
    Array (Class (1, []))
  ]
  
  
  ****************************************************************************************************
  
  3 (?) < Serializable : 
  [
    Class (3, []);
    Var {id=_.38, index=_.39, upb=Interface (3, []), lwb=_.41};
    Null;
    Intersect ([Interface (3, []) | _.94]);
    Array (Class (1, []));
    Intersect ([_.96 [=/= Interface (3, [])]; Interface (3, []) | _.165]);
    Intersect ([_.96 [=/= Interface (3, [])]; _.167 [=/= Interface (3, [])]; Interface (3, []) | _.194]);
    Intersect ([_.96 [=/= Interface (3, [])]; _.167 [=/= Interface (3, [])]; _.196 [=/= Interface (3, [])]; Interface (3, []) | _.219]);
    Intersect ([_.96 [=/= Interface (3, [])]; _.167 [=/= Interface (3, [])]; _.196 [=/= Interface (3, [])]; _.221 [=/= Interface (3, [])]; Interface (3, []) | _.247]);
    Intersect ([_.96 [=/= Interface (3, [])]; _.167 [=/= Interface (3, [])]; _.196 [=/= Interface (3, [])]; _.221 [=/= Interface (3, [])]; _.249 [=/= Interface (3, [])]; Interface (3, []) | _.271])
  ]
  
  
  ****************************************************************************************************
  
  4.1 (?) < Object[] : 
  [
    Null;
    Array (Class (1, []));
    Array (Var {id=_.113, index=_.114, upb=Class (1, []), lwb=_.116});
    Array (Interface (2, _.74));
    Array (Null);
    Array (Interface (3, _.74));
    Array (Intersect ([Class (1, []) | _.209]));
    Array (Array (Class (1, [])));
    Array (Intersect ([_.211 [=/= Class (1, [])]; Class (1, []) | _.302]));
    Array (Intersect ([_.211 [=/= Class (1, [])]; _.304 [=/= Class (1, [])]; Class (1, []) | _.328]))
  ]
  
  
  ****************************************************************************************************
  
  4.2 Object < (?) : 
  [
    Var {id=_.32, index=_.33, upb=_.34, lwb=Some (Class (1, []))};
    Interface (1, []);
    Class (1, []);
    Interface (0, []);
    Class (0, [])
  ]
  
  
  ****************************************************************************************************
  
  5 Cloneable < (?): 
  [
    Var {id=_.34, index=_.35, upb=_.36, lwb=Some (Interface (2, []))};
    Class (1, [])
  ]
  
  
  ****************************************************************************************************
  
  6 Serializable < (?) : 
  [
    Var {id=_.34, index=_.35, upb=_.36, lwb=Some (Interface (3, []))};
    Class (1, [])
  ]
  
  
  ****************************************************************************************************
  
  7.1 (?) < Serializable[] : 
  [
    Null;
    Array (Class (3, []));
    Array (Var {id=_.113, index=_.114, upb=Interface (3, []), lwb=_.116});
    Array (Null);
    Array (Intersect ([Interface (3, []) | _.219]));
    Array (Array (Class (1, [])));
    Array (Intersect ([_.221 [=/= Interface (3, [])]; Interface (3, []) | _.290]));
    Array (Intersect ([_.221 [=/= Interface (3, [])]; _.292 [=/= Interface (3, [])]; Interface (3, []) | _.319]));
    Array (Intersect ([_.221 [=/= Interface (3, [])]; _.292 [=/= Interface (3, [])]; _.321 [=/= Interface (3, [])]; Interface (3, []) | _.344]));
    Array (Intersect ([_.221 [=/= Interface (3, [])]; _.292 [=/= Interface (3, [])]; _.321 [=/= Interface (3, [])]; _.346 [=/= Interface (3, [])]; Interface (3, []) | _.372]))
  ]
  
  
  ****************************************************************************************************
  
  7.2 Object[][] < (?) : 
  [
    Array (Class (1, []));
    Array (Interface (2, []));
    Array (Interface (3, []));
    Array (Array (Var {id=_.100, index=_.101, upb=_.102, lwb=Some (Class (1, []))}));
    Array (Array (Interface (1, [])));
    Array (Array (Class (1, [])));
    Array (Array (Interface (0, [])));
    Array (Array (Class (0, [])))
  ]
  
  
  ****************************************************************************************************
  
  Class A: 15
  
  Class B: 16
  
  8.1 (?) < A : 
  [
    Null;
    Class (15, []);
    Class (16, _.15)
  ]
  
  
  ****************************************************************************************************
  
  8.2 B < (?) : 
  [
    Var {id=_.32, index=_.33, upb=_.34, lwb=Some (Class (16, []))};
    Interface (16, []);
    Class (16, []);
    Interface (15, []);
    Class (15, [])
  ]
  
  
  ****************************************************************************************************
  
  8.3 (?) < B : 
  [
    Null;
    Class (16, [])
  ]
  
  
  ****************************************************************************************************
  
  8.4 A < (?) : 
  [
    Var {id=_.32, index=_.33, upb=_.34, lwb=Some (Class (15, []))};
    Interface (15, []);
    Class (15, []);
    Interface (1, []);
    Class (1, [])
  ]
  
  
  ****************************************************************************************************
  
  Interface A: 20
  
  Class C: 21
  
  9 C < (?) : 
  [
    Var {id=_.32, index=_.33, upb=_.34, lwb=Some (Class (21, []))};
    Interface (21, []);
    Class (21, []);
    Interface (15, []);
    Class (15, []);
    Interface (20, []);
    Class (20, [])
  ]
  
  
  ****************************************************************************************************
  
  10.1 (?) < IA : 
  [
    Null;
    Class (20, []);
    Class (21, _.15)
  ]
  
  
  ****************************************************************************************************
  
  10.2 C < (?) : 
  [
    Var {id=_.32, index=_.33, upb=_.34, lwb=Some (Class (21, []))};
    Interface (21, []);
    Class (21, []);
    Interface (15, []);
    Class (15, []);
    Interface (20, []);
    Class (20, [])
  ]
  
  
  ****************************************************************************************************
  
  Interface B: 27
  
  11 IB < (?) : 
  [
    Var {id=_.34, index=_.35, upb=_.36, lwb=Some (Interface (27, []))};
    Class (27, []);
    Interface (27, []);
    Class (20, []);
    Interface (20, [])
  ]
  
  
  ****************************************************************************************************
  
  Class D<X>: 28
  
  Class E<X, Y>: 29
  
  Class F<X, Y>: 32
  
  12.1 (?) < E<D<B>, A> : 
  [
    Null;
    Class (32, [Type (Class (15, [])); Type (Class (16, []))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Class (_.24590, []))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Null)]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Interface (_.34469, []))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Array (Class (_.38386, [])))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Array (Null))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Intersect ([]))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Class (_.55939, [Wildcard (None)]))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Array (Interface (_.58669, [])))])
  ]
  
  
  ****************************************************************************************************
  
  12.2 (? - is class) < E<D<B>, A> : 
  [
    Class (32, [Type (Class (15, [])); Type (Class (16, []))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Class (_.24515, []))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Null)]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Interface (_.34394, []))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Array (Class (_.38311, [])))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Array (Null))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Intersect ([]))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Class (_.55864, [Wildcard (None)]))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Array (Interface (_.58594, [])))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, [])); Type (Array (Array (Class (_.64937, []))))])
  ]
  
  
  ****************************************************************************************************
  
  12.3 F<A, B> < (?) : 
  [
    Var {id=_.32, index=_.33, upb=_.34, lwb=Some (Class (32, [Type (Class (15, [])); Type (Class (16, []))]))};
    Interface (32, [Type (Class (15, [])); Type (Class (16, []))]);
    Class (32, [Type (Class (15, [])); Type (Class (16, []))]);
    Interface (32, [Type (Class (15, [])); Wildcard (Some ((Extends, Class (16, []))))]);
    Class (32, [Type (Class (15, [])); Wildcard (Some ((Extends, Class (16, []))))]);
    Interface (32, [Type (Class (15, [])); Wildcard (Some ((Super, Class (16, []))))]);
    Class (32, [Type (Class (15, [])); Wildcard (Some ((Super, Class (16, []))))]);
    Interface (32, [Wildcard (Some ((Extends, Class (15, [])))); Type (Class (16, []))]);
    Class (32, [Wildcard (Some ((Extends, Class (15, [])))); Type (Class (16, []))]);
    Interface (32, [Wildcard (Some ((Extends, Class (15, [])))); Wildcard (Some ((Extends, Class (16, []))))]);
    Class (32, [Wildcard (Some ((Extends, Class (15, [])))); Wildcard (Some ((Extends, Class (16, []))))]);
    Interface (32, [Wildcard (Some ((Super, Class (15, [])))); Type (Class (16, []))]);
    Class (32, [Wildcard (Some ((Super, Class (15, [])))); Type (Class (16, []))]);
    Interface (32, [Wildcard (Some ((Extends, Class (15, [])))); Wildcard (Some ((Super, Class (16, []))))]);
    Class (32, [Wildcard (Some ((Extends, Class (15, [])))); Wildcard (Some ((Super, Class (16, []))))]);
    Interface (32, [Wildcard (Some ((Super, Class (15, [])))); Wildcard (Some ((Extends, Class (16, []))))]);
    Class (32, [Wildcard (Some ((Super, Class (15, [])))); Wildcard (Some ((Extends, Class (16, []))))]);
    Interface (32, [Wildcard (Some ((Super, Class (15, [])))); Wildcard (Some ((Super, Class (16, []))))]);
    Class (32, [Wildcard (Some ((Super, Class (15, [])))); Wildcard (Some ((Super, Class (16, []))))]);
    Interface (29, [Type (Class (28, [Type (Class (16, []))])); Type (Class (15, []))]);
    Class (29, [Type (Class (28, [Type (Class (16, []))])); Type (Class (15, []))])
  ]

