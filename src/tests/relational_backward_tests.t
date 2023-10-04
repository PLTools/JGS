  $ ./relational_backward_tests.exe
  1.1 (?) < Object : 
  [
    Class (1, []);
    Array (Class (1, []));
    Interface (1, [])
  ]
  
  
  ****************************************************************************************************
  
  1.2 Object[] < (?) : 
  [
    Class (1, []);
    Interface (2, []);
    Interface (3, []);
    Array (Var {id=_.40, index=_.41, upb=_.42, lwb=Some (Class (1, []))});
    Array (Interface (1, []));
    Array (Class (1, []));
    Array (Interface (0, []));
    Array (Class (0, []))
  ]
  
  
  ****************************************************************************************************
  
  2 (?) < Cloneable : 
  [
    Class (2, []);
    Array (Class (1, []));
    Interface (2, [])
  ]
  
  
  ****************************************************************************************************
  
  3 (?) < Serializable : 
  [
    Class (3, []);
    Interface (3, []);
    Array (Class (1, []));
    Intersect ([Interface (3, []) | _.47]);
    Var {id=_.42, index=_.43, upb=Interface (3, []), lwb=_.44};
    Null;
    Intersect ([_.46 [=/= Interface (3, [])]; Interface (3, []) | _.55]);
    Intersect ([_.46 [=/= Interface (3, [])]; _.54 [=/= Interface (3, [])]; Interface (3, []) | _.57]);
    Intersect ([_.46 [=/= Interface (3, [])]; _.54 [=/= Interface (3, [])]; _.56 [=/= Interface (3, [])]; Interface (3, []) | _.59]);
    Intersect ([_.46 [=/= Interface (3, [])]; _.54 [=/= Interface (3, [])]; _.56 [=/= Interface (3, [])]; _.58 [=/= Interface (3, [])]; Interface (3, []) | _.61])
  ]
  
  
  ****************************************************************************************************
  
  4.1 (?) < Object[] : 
  [
    Array (Class (1, []));
    Array (Interface (1, []));
    Array (Class (3, _.52));
    Array (Class (2, _.52));
    Array (Array (Class (1, [])));
    Array (Intersect ([Class (1, []) | _.106]));
    Array (Var {id=_.97, index=_.98, upb=Class (1, []), lwb=_.99});
    Array (Null);
    Array (Intersect ([_.105 [=/= Class (1, [])]; Class (1, []) | _.134]));
    Array (Interface (3, _.61))
  ]
  
  
  ****************************************************************************************************
  
  4.2 Object < (?) : 
  [
    Var {id=_.27, index=_.28, upb=_.29, lwb=Some (Class (1, []))};
    Interface (1, []);
    Class (1, []);
    Interface (0, []);
    Class (0, [])
  ]
  
  
  ****************************************************************************************************
  
  5 Cloneable < (?): 
  [
    Var {id=_.27, index=_.28, upb=_.29, lwb=Some (Interface (2, []))};
    Class (2, []);
    Interface (2, []);
    Class (1, []);
    Interface (1, [])
  ]
  
  
  ****************************************************************************************************
  
  6 Serializable < (?) : 
  [
    Var {id=_.27, index=_.28, upb=_.29, lwb=Some (Interface (3, []))};
    Class (3, []);
    Interface (3, []);
    Class (1, []);
    Interface (1, [])
  ]
  
  
  ****************************************************************************************************
  
  7.1 (?) < Serializable[] : 
  [
    Array (Class (3, []));
    Array (Interface (3, []));
    Array (Array (Class (1, [])));
    Array (Intersect ([Interface (3, []) | _.96]));
    Array (Var {id=_.91, index=_.92, upb=Interface (3, []), lwb=_.93});
    Array (Null);
    Array (Intersect ([_.95 [=/= Interface (3, [])]; Interface (3, []) | _.104]));
    Array (Intersect ([_.95 [=/= Interface (3, [])]; _.103 [=/= Interface (3, [])]; Interface (3, []) | _.106]));
    Array (Intersect ([_.95 [=/= Interface (3, [])]; _.103 [=/= Interface (3, [])]; _.105 [=/= Interface (3, [])]; Interface (3, []) | _.108]));
    Array (Intersect ([_.95 [=/= Interface (3, [])]; _.103 [=/= Interface (3, [])]; _.105 [=/= Interface (3, [])]; _.107 [=/= Interface (3, [])]; Interface (3, []) | _.110]))
  ]
  
  
  ****************************************************************************************************
  
  7.2 Object[][] < (?) : 
  [
    Array (Class (1, []));
    Array (Interface (2, []));
    Array (Interface (3, []));
    Array (Array (Var {id=_.53, index=_.54, upb=_.55, lwb=Some (Class (1, []))}));
    Array (Array (Interface (1, [])));
    Array (Array (Class (1, [])));
    Array (Array (Interface (0, [])));
    Array (Array (Class (0, [])))
  ]
  
  
  ****************************************************************************************************
  
  Class A: 4
  
  Class B: 5
  
  8.1 (?) < A : 
  [
    Class (4, []);
    Class (5, _.18);
    Interface (4, [])
  ]
  
  
  ****************************************************************************************************
  
  8.2 B < (?) : 
  [
    Var {id=_.27, index=_.28, upb=_.29, lwb=Some (Class (5, []))};
    Interface (5, []);
    Class (5, []);
    Interface (4, []);
    Class (4, [])
  ]
  
  
  ****************************************************************************************************
  
  8.3 (?) < B : 
  [
    Class (5, []);
    Interface (5, [])
  ]
  
  
  ****************************************************************************************************
  
  8.4 A < (?) : 
  [
    Var {id=_.27, index=_.28, upb=_.29, lwb=Some (Class (4, []))};
    Interface (4, []);
    Class (4, []);
    Interface (1, []);
    Class (1, [])
  ]
  
  
  ****************************************************************************************************
  
  Interface A: 6
  
  Class C: 7
  
  9 C < (?) : 
  [
    Var {id=_.27, index=_.28, upb=_.29, lwb=Some (Class (7, []))};
    Interface (7, []);
    Class (7, []);
    Interface (4, []);
    Class (4, []);
    Interface (6, []);
    Class (6, [])
  ]
  
  
  ****************************************************************************************************
  
  10.1 (?) < IA : 
  [
    Class (6, []);
    Class (7, _.18);
    Interface (6, [])
  ]
  
  
  ****************************************************************************************************
  
  10.2 C < (?) : 
  [
    Var {id=_.27, index=_.28, upb=_.29, lwb=Some (Class (7, []))};
    Interface (7, []);
    Class (7, []);
    Interface (4, []);
    Class (4, []);
    Interface (6, []);
    Class (6, [])
  ]
  
  
  ****************************************************************************************************
  
  Interface B: 8
  
  11 IB < (?) : 
  [
    Var {id=_.27, index=_.28, upb=_.29, lwb=Some (Interface (8, []))};
    Class (8, []);
    Interface (8, []);
    Class (6, []);
    Interface (6, [])
  ]
  
  
  ****************************************************************************************************
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  12.1 (?) < E<D<B>, A> : 
  [
    Class (10, [Type (Class (9, [Type (Class (5, []))])); Type (Class (4, []))]);
    Interface (10, [Type (Class (9, [Type (Class (5, []))])); Type (Class (4, []))]);
    Class (13, [Type (Class (4, [])); Type (Class (5, [])) | _.166]);
    Interface (13, [Type (Class (4, [])); Type (Class (5, [])) | _.221])
  ]
  
  
  ****************************************************************************************************
  
  12.2 (? - is class) < E<D<B>, A> : 
  [
    Class (10, [Type (Class (9, [Type (Class (5, []))])); Type (Class (4, []))]);
    Class (13, [Type (Class (4, [])); Type (Class (5, [])) | _.110])
  ]
  
  
  ****************************************************************************************************
  
  12.3 F<A, B> < (?) : 
  [
    Var {id=_.27, index=_.28, upb=_.29, lwb=Some (Class (13, [Type (Class (4, [])); Type (Class (5, []))]))};
    Interface (13, [Type (Class (4, [])); Type (Class (5, []))]);
    Class (13, [Type (Class (4, [])); Type (Class (5, []))]);
    Interface (10, [Type (Class (9, [Type (Class (5, []))])); Type (Class (4, []))]);
    Class (10, [Type (Class (9, [Type (Class (5, []))])); Type (Class (4, []))])
  ]

