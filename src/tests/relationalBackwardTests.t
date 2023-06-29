  $ ./relationalBackwardTests.exe
  1.1 (?) < Object : 
  [
    Array (Class (1, []));
    Class (1, []);
    Interface (1, [])
  ]
  
  
  ****************************************************************************************************
  
  1.2 Object[] < (?) : 
  [
    Class (1, []);
    Interface (2, []);
    Interface (3, []);
    Array (Var {id=_.75, index=_.76, upb=_.77, lwb=Some (Class (1, []))});
    Array (Interface (1, []));
    Array (Class (1, []))
  ]
  
  
  ****************************************************************************************************
  
  2 (?) < Cloneable : 
  [
    Array (Class (1, []));
    Class (2, []);
    Interface (2, [])
  ]
  
  
  ****************************************************************************************************
  
  3 (?) < Serializable : 
  [
    Class (3, []);
    Intersect ([Interface (3, []) | _.80]);
    Var {id=_.63, index=_.64, upb=Interface (3, []), lwb=_.65};
    Array (Class (1, []));
    Null;
    Intersect ([_.82 [=/= Interface (3, [])]; Interface (3, []) | _.224]);
    Intersect ([_.82 [=/= Interface (3, [])]; _.226 [=/= Interface (3, [])]; Interface (3, []) | _.246]);
    Interface (3, []);
    Intersect ([_.82 [=/= Interface (3, [])]; _.226 [=/= Interface (3, [])]; _.248 [=/= Interface (3, [])]; Interface (3, []) | _.299]);
    Intersect ([_.82 [=/= Interface (3, [])]; _.226 [=/= Interface (3, [])]; _.248 [=/= Interface (3, [])]; _.301 [=/= Interface (3, [])]; Interface (3, []) | _.329])
  ]
  
  
  ****************************************************************************************************
  
  4.1 (?) < Object[] : 
  [
    Array (Class (1, []));
    Array (Array (Class (1, [])));
    Array (Intersect ([Class (1, []) | _.734]));
    Array (Var {id=_.635, index=_.636, upb=Class (1, []), lwb=_.637});
    Array (Null);
    Array (Intersect ([_.736 [=/= Class (1, [])]; Class (1, []) | _.1215]));
    Array (Class (2, []));
    Array (Class (3, []));
    Array (Intersect ([_.736 [=/= Class (1, [])]; _.1217 [=/= Class (1, [])]; Class (1, []) | _.1396]));
    Array (Intersect ([_.736 [=/= Class (1, [])]; _.1217 [=/= Class (1, [])]; _.1398 [=/= Class (1, [])]; Class (1, []) | _.1559]))
  ]
  
  
  ****************************************************************************************************
  
  4.2 Object < (?) : 
  [
    Var {id=_.56, index=_.57, upb=_.58, lwb=Some (Class (1, []))};
    Interface (1, []);
    Class (1, [])
  ]
  
  
  ****************************************************************************************************
  
  5 Cloneable < (?): 
  [
    Var {id=_.55, index=_.56, upb=_.57, lwb=Some (Interface (2, []))};
    Class (2, []);
    Interface (2, []);
    Class (1, []);
    Interface (1, [])
  ]
  
  
  ****************************************************************************************************
  
  6 Serializable < (?) : 
  [
    Var {id=_.55, index=_.56, upb=_.57, lwb=Some (Interface (3, []))};
    Class (3, []);
    Interface (3, []);
    Class (1, []);
    Interface (1, [])
  ]
  
  
  ****************************************************************************************************
  
  7.1 (?) < Serializable[] : 
  [
    Array (Class (3, []));
    Array (Intersect ([Interface (3, []) | _.720]));
    Array (Var {id=_.635, index=_.636, upb=Interface (3, []), lwb=_.637});
    Array (Array (Class (1, [])));
    Array (Null);
    Array (Intersect ([_.722 [=/= Interface (3, [])]; Interface (3, []) | _.1365]));
    Array (Intersect ([_.722 [=/= Interface (3, [])]; _.1367 [=/= Interface (3, [])]; Interface (3, []) | _.1514]));
    Array (Interface (3, []));
    Array (Intersect ([_.722 [=/= Interface (3, [])]; _.1367 [=/= Interface (3, [])]; _.1516 [=/= Interface (3, [])]; Interface (3, []) | _.1707]));
    Array (Intersect ([_.722 [=/= Interface (3, [])]; _.1367 [=/= Interface (3, [])]; _.1516 [=/= Interface (3, [])]; _.1709 [=/= Interface (3, [])]; Interface (3, []) | _.1880]))
  ]
  
  
  ****************************************************************************************************
  
  7.2 Object[][] < (?) : 
  [
    Array (Class (1, []));
    Array (Interface (2, []));
    Array (Interface (3, []));
    Array (Array (Var {id=_.92, index=_.93, upb=_.94, lwb=Some (Class (1, []))}));
    Array (Array (Interface (1, [])));
    Array (Array (Class (1, [])))
  ]
  
  
  ****************************************************************************************************
  
  Class A: 119
  
  Class B: 120
  
  8.1 (?) < A : 
  [
    Class (119, []);
    Class (120, []);
    Interface (119, [])
  ]
  
  
  ****************************************************************************************************
  
  8.2 B < (?) : 
  [
    Var {id=_.56, index=_.57, upb=_.58, lwb=Some (Class (120, []))};
    Interface (120, []);
    Class (120, []);
    Interface (119, []);
    Class (119, [])
  ]
  
  
  ****************************************************************************************************
  
  8.3 (?) < B : 
  [
    Class (120, []);
    Interface (120, [])
  ]
  
  
  ****************************************************************************************************
  
  8.4 A < (?) : 
  [
    Var {id=_.56, index=_.57, upb=_.58, lwb=Some (Class (119, []))};
    Interface (119, []);
    Class (119, []);
    Interface (1, []);
    Class (1, [])
  ]
  
  
  ****************************************************************************************************
  
  Interface A: 133
  
  Class C: 134
  
  9 C < (?) : 
  [
    Var {id=_.56, index=_.57, upb=_.58, lwb=Some (Class (134, []))};
    Interface (134, []);
    Class (134, []);
    Interface (119, []);
    Class (119, []);
    Interface (133, []);
    Class (133, [])
  ]
  
  
  ****************************************************************************************************
  
  10.1 (?) < IA : 
  [
    Class (133, []);
    Class (134, []);
    Interface (133, [])
  ]
  
  
  ****************************************************************************************************
  
  10.2 C < (?) : 
  [
    Var {id=_.56, index=_.57, upb=_.58, lwb=Some (Class (134, []))};
    Interface (134, []);
    Class (134, []);
    Interface (119, []);
    Class (119, []);
    Interface (133, []);
    Class (133, [])
  ]
  
  
  ****************************************************************************************************
  
  Interface B: 141
  
  11 IB < (?) : 
  [
    Var {id=_.55, index=_.56, upb=_.57, lwb=Some (Interface (141, []))};
    Class (141, []);
    Interface (141, []);
    Class (133, []);
    Interface (133, [])
  ]
  
  
  ****************************************************************************************************
  
  Class D<X>: 142
  
  Class E<X, Y>: 143
  
  Class F<X, Y>: 146
  
  12.1 (?) < E<D<B>, A> : 
  [
    Class (146, [Type (Class (119, [])); Type (Class (120, []))]);
    Interface (146, [Type (Class (119, [])); Type (Class (120, []))]);
    Interface (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Class (_.36880, []))]);
    Class (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Class (_.37039, []))]);
    Interface (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Interface (_.49012, []))]);
    Class (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Interface (_.49242, []))]);
    Interface (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Array (Class (_.53358, [])))]);
    Class (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Array (Class (_.53600, [])))]);
    Interface (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Null)]);
    Class (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Null)])
  ]
  
  
  ****************************************************************************************************
  
  12.2 (? - is class) < E<D<B>, A> : 
  [
    Class (146, [Type (Class (119, [])); Type (Class (120, []))]);
    Class (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Class (_.18497, []))]);
    Class (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Interface (_.24581, []))]);
    Class (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Array (Class (_.26758, [])))]);
    Class (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Null)]);
    Class (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Intersect ([]))]);
    Class (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Array (Interface (_.38232, [])))]);
    Class (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Array (Array (Class (_.42783, []))))]);
    Class (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Array (Null))]);
    Class (146, [Type (Class (119, [])); Type (Class (120, [])); Type (Class (_.56270, [Wildcard (None)]))])
  ]
  
  
  ****************************************************************************************************
  
  12.3 F<A, B> < (?) : 
  [
    Var {id=_.288, index=_.289, upb=_.290, lwb=Some (Class (146, [Type (Class (119, [])); Type (Class (120, []))]))};
    Interface (143, [Type (Class (142, [Type (Class (120, []))])); Type (Class (119, []))]);
    Class (143, [Type (Class (142, [Type (Class (120, []))])); Type (Class (119, []))]);
    Interface (146, [Type (Class (119, [])); Type (Class (120, []))]);
    Class (146, [Type (Class (119, [])); Type (Class (120, []))]);
    Interface (146, [Type (Class (119, [])); Wildcard (Some ((Extends, Class (120, []))))]);
    Class (146, [Type (Class (119, [])); Wildcard (Some ((Extends, Class (120, []))))]);
    Interface (146, [Type (Class (119, [])); Wildcard (Some ((Super, Class (120, []))))]);
    Class (146, [Type (Class (119, [])); Wildcard (Some ((Super, Class (120, []))))]);
    Interface (146, [Wildcard (Some ((Extends, Class (119, [])))); Type (Class (120, []))]);
    Class (146, [Wildcard (Some ((Extends, Class (119, [])))); Type (Class (120, []))]);
    Interface (146, [Wildcard (Some ((Extends, Class (119, [])))); Wildcard (Some ((Extends, Class (120, []))))]);
    Class (146, [Wildcard (Some ((Extends, Class (119, [])))); Wildcard (Some ((Extends, Class (120, []))))]);
    Interface (146, [Wildcard (Some ((Extends, Class (119, [])))); Wildcard (Some ((Super, Class (120, []))))]);
    Class (146, [Wildcard (Some ((Extends, Class (119, [])))); Wildcard (Some ((Super, Class (120, []))))]);
    Interface (146, [Wildcard (Some ((Super, Class (119, [])))); Type (Class (120, []))]);
    Class (146, [Wildcard (Some ((Super, Class (119, [])))); Type (Class (120, []))]);
    Interface (146, [Wildcard (Some ((Super, Class (119, [])))); Wildcard (Some ((Extends, Class (120, []))))]);
    Class (146, [Wildcard (Some ((Super, Class (119, [])))); Wildcard (Some ((Extends, Class (120, []))))]);
    Interface (146, [Wildcard (Some ((Super, Class (119, [])))); Wildcard (Some ((Super, Class (120, []))))]);
    Class (146, [Wildcard (Some ((Super, Class (119, [])))); Wildcard (Some ((Super, Class (120, []))))])
  ]

