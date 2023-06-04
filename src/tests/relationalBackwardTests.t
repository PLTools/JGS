  $ ./relationalBackwardTests.exe
  1.1 (?) < Object : 
  [
    Null;
    Array (Class (1, []));
    Class (1, []);
    Interface (2, []);
    Interface (3, [])
  ]
  
  
  ****************************************************************************************************
  
  1.2 Object[] < (?) : 
  [
    Class (1, []);
    Interface (2, []);
    Interface (3, []);
    Array (Var {id=_.109, index=_.110, upb=_.111, lwb=Some (Class (1, []))});
    Array (Interface (1, []));
    Array (Class (1, []));
    Array (Interface (0, []));
    Array (Class (0, []))
  ]
  
  
  ****************************************************************************************************
  
  2 (?) < Cloneable : 
  [
    Null;
    Array (Class (1, []));
    Class (2, [])
  ]
  
  
  ****************************************************************************************************
  
  3 (?) < Serializable : 
  [
    Var {id=_.34, index=_.35, upb=Interface (3, []), lwb=_.37};
    Null;
    Intersect ([Interface (3, []) | _.104]);
    Class (3, []);
    Array (Class (1, []));
    Intersect ([_.106 [=/= Interface (3, [])]; Interface (3, []) | _.232]);
    Intersect ([_.106 [=/= Interface (3, [])]; _.234 [=/= Interface (3, [])]; Interface (3, []) | _.304]);
    Intersect ([_.106 [=/= Interface (3, [])]; _.234 [=/= Interface (3, [])]; _.306 [=/= Interface (3, [])]; Interface (3, []) | _.344]);
    Intersect ([_.106 [=/= Interface (3, [])]; _.234 [=/= Interface (3, [])]; _.306 [=/= Interface (3, [])]; _.346 [=/= Interface (3, [])]; Interface (3, []) | _.374]);
    Intersect ([_.106 [=/= Interface (3, [])]; _.234 [=/= Interface (3, [])]; _.306 [=/= Interface (3, [])]; _.346 [=/= Interface (3, [])]; _.376 [=/= Interface (3, [])]; Interface (3, []) | _.416])
  ]
  
  
  ****************************************************************************************************
  
  4.1 (?) < Object[] : 
  [
    Null;
    Array (Class (1, []));
    Array (Var {id=_.399, index=_.400, upb=Class (1, []), lwb=_.402});
    Array (Null);
    Array (Intersect ([Class (1, []) | _.910]));
    Array (Array (Class (1, [])));
    Array (Intersect ([_.912 [=/= Class (1, [])]; Class (1, []) | _.1501]));
    Array (Intersect ([_.912 [=/= Class (1, [])]; _.1503 [=/= Class (1, [])]; Class (1, []) | _.1726]));
    Array (Intersect ([_.912 [=/= Class (1, [])]; _.1503 [=/= Class (1, [])]; _.1728 [=/= Class (1, [])]; Class (1, []) | _.1903]));
    Array (Intersect ([_.912 [=/= Class (1, [])]; _.1503 [=/= Class (1, [])]; _.1728 [=/= Class (1, [])]; _.1905 [=/= Class (1, [])]; Class (1, []) | _.2091]))
  ]
  
  
  ****************************************************************************************************
  
  4.2 Object < (?) : 
  [
    Var {id=_.66, index=_.67, upb=_.68, lwb=Some (Class (1, []))};
    Interface (1, []);
    Class (1, []);
    Interface (0, []);
    Class (0, [])
  ]
  
  
  ****************************************************************************************************
  
  5 Cloneable < (?): 
  [
    Var {id=_.66, index=_.67, upb=_.68, lwb=Some (Interface (2, []))};
    Class (1, [])
  ]
  
  
  ****************************************************************************************************
  
  6 Serializable < (?) : 
  [
    Var {id=_.66, index=_.67, upb=_.68, lwb=Some (Interface (3, []))};
    Class (1, [])
  ]
  
  
  ****************************************************************************************************
  
  7.1 (?) < Serializable[] : 
  [
    Null;
    Array (Var {id=_.399, index=_.400, upb=Interface (3, []), lwb=_.402});
    Array (Null);
    Array (Intersect ([Interface (3, []) | _.956]));
    Array (Class (3, []));
    Array (Array (Class (1, [])));
    Array (Intersect ([_.958 [=/= Interface (3, [])]; Interface (3, []) | _.1635]));
    Array (Intersect ([_.958 [=/= Interface (3, [])]; _.1637 [=/= Interface (3, [])]; Interface (3, []) | _.1949]));
    Array (Intersect ([_.958 [=/= Interface (3, [])]; _.1637 [=/= Interface (3, [])]; _.1951 [=/= Interface (3, [])]; Interface (3, []) | _.2142]));
    Array (Intersect ([_.958 [=/= Interface (3, [])]; _.1637 [=/= Interface (3, [])]; _.1951 [=/= Interface (3, [])]; _.2144 [=/= Interface (3, [])]; Interface (3, []) | _.2328]))
  ]
  
  
  ****************************************************************************************************
  
  7.2 Object[][] < (?) : 
  [
    Array (Class (1, []));
    Array (Interface (2, []));
    Array (Interface (3, []));
    Array (Array (Var {id=_.138, index=_.139, upb=_.140, lwb=Some (Class (1, []))}));
    Array (Array (Interface (1, [])));
    Array (Array (Class (1, [])));
    Array (Array (Interface (0, [])));
    Array (Array (Class (0, [])))
  ]
  
  
  ****************************************************************************************************
  
  Class A: 124
  
  Class B: 125
  
  8.1 (?) < A : 
  [
    Null;
    Class (124, []);
    Class (125, [])
  ]
  
  
  ****************************************************************************************************
  
  8.2 B < (?) : 
  [
    Var {id=_.66, index=_.67, upb=_.68, lwb=Some (Class (125, []))};
    Interface (125, []);
    Class (125, []);
    Interface (124, []);
    Class (124, [])
  ]
  
  
  ****************************************************************************************************
  
  8.3 (?) < B : 
  [
    Null;
    Class (125, [])
  ]
  
  
  ****************************************************************************************************
  
  8.4 A < (?) : 
  [
    Var {id=_.66, index=_.67, upb=_.68, lwb=Some (Class (124, []))};
    Interface (124, []);
    Class (124, []);
    Interface (1, []);
    Class (1, [])
  ]
  
  
  ****************************************************************************************************
  
  Interface A: 138
  
  Class C: 139
  
  9 C < (?) : 
  [
    Var {id=_.66, index=_.67, upb=_.68, lwb=Some (Class (139, []))};
    Interface (139, []);
    Class (139, []);
    Interface (124, []);
    Class (124, []);
    Interface (138, []);
    Class (138, [])
  ]
  
  
  ****************************************************************************************************
  
  10.1 (?) < IA : 
  [
    Null;
    Class (138, []);
    Class (139, [])
  ]
  
  
  ****************************************************************************************************
  
  10.2 C < (?) : 
  [
    Var {id=_.66, index=_.67, upb=_.68, lwb=Some (Class (139, []))};
    Interface (139, []);
    Class (139, []);
    Interface (124, []);
    Class (124, []);
    Interface (138, []);
    Class (138, [])
  ]
  
  
  ****************************************************************************************************
  
  Interface B: 154
  
  11 IB < (?) : 
  [
    Var {id=_.66, index=_.67, upb=_.68, lwb=Some (Interface (154, []))};
    Class (154, []);
    Interface (154, []);
    Class (138, []);
    Interface (138, [])
  ]
  
  
  ****************************************************************************************************
  
  Class D<X>: 155
  
  Class E<X, Y>: 156
  
  Class F<X, Y>: 159
  
  12.1 (?) < E<D<B>, A> : 
  [
    Null;
    Class (159, [Type (Class (124, [])); Type (Class (125, []))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Class (_.66810, []))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Null)]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Interface (_.127018, []))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Array (Class (_.169403, [])))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Array (Null))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Intersect ([]))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Class (_.344167, [Wildcard (None)]))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Array (Interface (_.391763, [])))])
  ]
  
  
  ****************************************************************************************************
  
  12.2 (? - is class) < E<D<B>, A> : 
  [
    Class (159, [Type (Class (124, [])); Type (Class (125, []))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Class (_.35404, []))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Null)]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Interface (_.68259, []))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Array (Class (_.91249, [])))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Array (Null))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Intersect ([]))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Class (_.190251, [Wildcard (None)]))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Array (Interface (_.217114, [])))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, [])); Type (Array (Array (Class (_.625883, []))))])
  ]
  
  
  ****************************************************************************************************
  
  12.3 F<A, B> < (?) : 
  [
    Var {id=_.372, index=_.373, upb=_.374, lwb=Some (Class (159, [Type (Class (124, [])); Type (Class (125, []))]))};
    Interface (159, [Type (Class (124, [])); Type (Class (125, []))]);
    Class (159, [Type (Class (124, [])); Type (Class (125, []))]);
    Interface (159, [Type (Class (124, [])); Wildcard (Some ((Extends, Class (125, []))))]);
    Class (159, [Type (Class (124, [])); Wildcard (Some ((Extends, Class (125, []))))]);
    Interface (159, [Type (Class (124, [])); Wildcard (Some ((Super, Class (125, []))))]);
    Class (159, [Type (Class (124, [])); Wildcard (Some ((Super, Class (125, []))))]);
    Interface (159, [Wildcard (Some ((Extends, Class (124, [])))); Type (Class (125, []))]);
    Class (159, [Wildcard (Some ((Extends, Class (124, [])))); Type (Class (125, []))]);
    Interface (159, [Wildcard (Some ((Extends, Class (124, [])))); Wildcard (Some ((Extends, Class (125, []))))]);
    Class (159, [Wildcard (Some ((Extends, Class (124, [])))); Wildcard (Some ((Extends, Class (125, []))))]);
    Interface (159, [Wildcard (Some ((Super, Class (124, [])))); Type (Class (125, []))]);
    Class (159, [Wildcard (Some ((Super, Class (124, [])))); Type (Class (125, []))]);
    Interface (159, [Wildcard (Some ((Extends, Class (124, [])))); Wildcard (Some ((Super, Class (125, []))))]);
    Class (159, [Wildcard (Some ((Extends, Class (124, [])))); Wildcard (Some ((Super, Class (125, []))))]);
    Interface (159, [Wildcard (Some ((Super, Class (124, [])))); Wildcard (Some ((Extends, Class (125, []))))]);
    Class (159, [Wildcard (Some ((Super, Class (124, [])))); Wildcard (Some ((Extends, Class (125, []))))]);
    Interface (159, [Wildcard (Some ((Super, Class (124, [])))); Wildcard (Some ((Super, Class (125, []))))]);
    Class (159, [Wildcard (Some ((Super, Class (124, [])))); Wildcard (Some ((Super, Class (125, []))))]);
    Interface (156, [Type (Class (155, [Type (Class (125, []))])); Type (Class (124, []))]);
    Class (156, [Type (Class (155, [Type (Class (125, []))])); Type (Class (124, []))])
  ]

