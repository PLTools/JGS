  $ timeout 2 ./relational_backward_tests.exe -1_1
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  1.1 (?) < Object : 
  [
    Array (Class (1, []));
    Class (1, []);
    Interface (1, [])
  ]
  
  
  ****************************************************************************************************
  


  $ timeout 2 ./relational_backward_tests.exe -1_2
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  1.2 Object[] < (?) : 
  [
    Class (1, []);
    Interface (2, []);
    Interface (3, []);
    Array (Var {id=_.72, index=_.73, upb=_.74, lwb=Some (Class (1, []))});
    Array (Interface (1, []));
    Array (Class (1, []));
    Array (Interface (0, []));
    Array (Class (0, []))
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -2
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  2 (?) < Cloneable : 
  [
    Array (Class (1, []));
    Class (2, []);
    Interface (2, [])
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -3
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  3 (?) < Serializable : 
  [
    Class (3, []);
    Intersect ([Interface (3, []) | _.83]);
    Var {id=_.68, index=_.69, upb=Interface (3, []), lwb=_.70};
    Array (Class (1, []));
    Null;
    Intersect ([_.85 [=/= Interface (3, [])]; Interface (3, []) | _.221]);
    Intersect ([_.85 [=/= Interface (3, [])]; _.223 [=/= Interface (3, [])]; Interface (3, []) | _.254]);
    Interface (3, []);
    Intersect ([_.85 [=/= Interface (3, [])]; _.223 [=/= Interface (3, [])]; _.256 [=/= Interface (3, [])]; Interface (3, []) | _.289]);
    Intersect ([_.85 [=/= Interface (3, [])]; _.223 [=/= Interface (3, [])]; _.256 [=/= Interface (3, [])]; _.291 [=/= Interface (3, [])]; Interface (3, []) | _.324])
  ]
  
  
  ****************************************************************************************************
  


  $ timeout 2 ./relational_backward_tests.exe -4_1
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  4.1 (?) < Object[] : 
  [
    Array (Class (1, []));
    Array (Array (Class (1, [])));
    Array (Intersect ([Class (1, []) | _.709]));
    Array (Var {id=_.621, index=_.622, upb=Class (1, []), lwb=_.623});
    Array (Null);
    Array (Intersect ([_.711 [=/= Class (1, [])]; Class (1, []) | _.1148]));
    Array (Class (10, []));
    Array (Intersect ([_.711 [=/= Class (1, [])]; _.1150 [=/= Class (1, [])]; Class (1, []) | _.1303]));
    Array (Intersect ([_.711 [=/= Class (1, [])]; _.1150 [=/= Class (1, [])]; _.1305 [=/= Class (1, [])]; Class (1, []) | _.1468]));
    Array (Class (9, []))
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -4_2
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  4.2 Object < (?) : 
  [
    Var {id=_.55, index=_.56, upb=_.57, lwb=Some (Class (1, []))};
    Interface (1, []);
    Class (1, []);
    Interface (0, []);
    Class (0, [])
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -5
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  5 Cloneable < (?): 
  [
    Var {id=_.54, index=_.55, upb=_.56, lwb=Some (Interface (2, []))};
    Class (2, []);
    Interface (2, []);
    Class (1, []);
    Interface (1, [])
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -6
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  6 Serializable < (?) : 
  [
    Var {id=_.54, index=_.55, upb=_.56, lwb=Some (Interface (3, []))};
    Class (3, []);
    Interface (3, []);
    Class (1, []);
    Interface (1, [])
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -7_1
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  7.1 (?) < Serializable[] : 
  [
    Array (Class (3, []));
    Array (Intersect ([Interface (3, []) | _.687]));
    Array (Var {id=_.621, index=_.622, upb=Interface (3, []), lwb=_.623});
    Array (Array (Class (1, [])));
    Array (Null);
    Array (Intersect ([_.689 [=/= Interface (3, [])]; Interface (3, []) | _.1277]));
    Array (Intersect ([_.689 [=/= Interface (3, [])]; _.1279 [=/= Interface (3, [])]; Interface (3, []) | _.1437]));
    Array (Interface (3, []));
    Array (Intersect ([_.689 [=/= Interface (3, [])]; _.1279 [=/= Interface (3, [])]; _.1439 [=/= Interface (3, [])]; Interface (3, []) | _.1578]));
    Array (Intersect ([_.689 [=/= Interface (3, [])]; _.1279 [=/= Interface (3, [])]; _.1439 [=/= Interface (3, [])]; _.1580 [=/= Interface (3, [])]; Interface (3, []) | _.1736]))
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -7_2
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  7.2 Object[][] < (?) : 
  [
    Array (Class (1, []));
    Array (Interface (2, []));
    Array (Interface (3, []));
    Array (Array (Var {id=_.87, index=_.88, upb=_.89, lwb=Some (Class (1, []))}));
    Array (Array (Interface (1, [])));
    Array (Array (Class (1, [])));
    Array (Array (Interface (0, [])));
    Array (Array (Class (0, [])))
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -8_1
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  8.1 (?) < A : 
  [
    Class (4, []);
    Interface (4, []);
    Class (7, [])
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -8_2
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  8.2 B < (?) : 
  [
    Var {id=_.55, index=_.56, upb=_.57, lwb=Some (Class (5, []))};
    Interface (5, []);
    Class (5, []);
    Interface (4, []);
    Class (4, [])
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -8_3
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  8.3 (?) < B : 
  [
    Class (5, []);
    Interface (5, [])
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -8_4
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  8.4 A < (?) : 
  [
    Var {id=_.55, index=_.56, upb=_.57, lwb=Some (Class (4, []))};
    Interface (4, []);
    Class (4, []);
    Interface (1, []);
    Class (1, [])
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -9
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  9 C < (?) : 
  [
    Var {id=_.55, index=_.56, upb=_.57, lwb=Some (Class (7, []))};
    Interface (7, []);
    Class (7, []);
    Interface (4, []);
    Class (4, []);
    Interface (6, []);
    Class (6, [])
  ]
  
  
  ****************************************************************************************************
  
  $ timeout 2 ./relational_backward_tests.exe -10_1
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  10.1 (?) < IA : 
  [
    Class (6, []);
    Interface (6, []);
    Class (8, [])
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -10_2
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  10.2 C < (?) : 
  [
    Var {id=_.55, index=_.56, upb=_.57, lwb=Some (Class (7, []))};
    Interface (7, []);
    Class (7, []);
    Interface (4, []);
    Class (4, []);
    Interface (6, []);
    Class (6, [])
  ]
  
  
  ****************************************************************************************************
  

  $ timeout 2 ./relational_backward_tests.exe -11
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  11 IB < (?) : 
  [
    Var {id=_.54, index=_.55, upb=_.56, lwb=Some (Interface (8, []))};
    Class (8, []);
    Interface (8, []);
    Class (6, []);
    Interface (6, [])
  ]
  
  
  ****************************************************************************************************
  


  $ timeout 2 ./relational_backward_tests.exe -12_1
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  12.1 (?) < E<D<B>, A> : 
  [
    Interface (13, [Type (Class (4, [])); Type (Class (5, []))]);
    Class (13, [Type (Class (4, [])); Type (Class (5, []))])
  ]
  
  
  ****************************************************************************************************
  
  $ timeout 2 ./relational_backward_tests.exe -12_2
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  12.2 (? - is class) < E<D<B>, A> : 
  [
    Class (13, [Type (Class (4, [])); Type (Class (5, []))])
  ]
  
  
  ****************************************************************************************************
  
  $ timeout 2 ./relational_backward_tests.exe -12_3
  Class A: 4
  
  Class B: 5
  
  Interface A: 6
  
  Class C: 7
  
  Interface B: 8
  
  Class D<X>: 9
  
  Class E<X, Y>: 10
  
  Class F<X, Y>: 13
  
  12.3 F<A, B> < (?) : 
  [
    Var {id=_.307, index=_.308, upb=_.309, lwb=Some (Class (13, [Type (Class (4, [])); Type (Class (5, []))]))};
    Interface (10, [Type (Class (9, [Type (Class (5, []))])); Type (Class (4, []))]);
    Class (10, [Type (Class (9, [Type (Class (5, []))])); Type (Class (4, []))]);
    Interface (13, [Type (Class (4, [])); Type (Class (5, []))]);
    Class (13, [Type (Class (4, [])); Type (Class (5, []))]);
    Interface (13, [Type (Class (4, [])); Wildcard (Some ((Extends, Class (5, []))))]);
    Class (13, [Type (Class (4, [])); Wildcard (Some ((Extends, Class (5, []))))]);
    Interface (13, [Type (Class (4, [])); Wildcard (Some ((Super, Class (5, []))))]);
    Class (13, [Type (Class (4, [])); Wildcard (Some ((Super, Class (5, []))))]);
    Interface (13, [Wildcard (Some ((Extends, Class (4, [])))); Type (Class (5, []))]);
    Class (13, [Wildcard (Some ((Extends, Class (4, [])))); Type (Class (5, []))]);
    Interface (13, [Wildcard (Some ((Extends, Class (4, [])))); Wildcard (Some ((Extends, Class (5, []))))]);
    Class (13, [Wildcard (Some ((Extends, Class (4, [])))); Wildcard (Some ((Extends, Class (5, []))))]);
    Interface (13, [Wildcard (Some ((Extends, Class (4, [])))); Wildcard (Some ((Super, Class (5, []))))]);
    Class (13, [Wildcard (Some ((Extends, Class (4, [])))); Wildcard (Some ((Super, Class (5, []))))]);
    Interface (13, [Wildcard (Some ((Super, Class (4, [])))); Type (Class (5, []))]);
    Class (13, [Wildcard (Some ((Super, Class (4, [])))); Type (Class (5, []))]);
    Interface (13, [Wildcard (Some ((Super, Class (4, [])))); Wildcard (Some ((Extends, Class (5, []))))]);
    Class (13, [Wildcard (Some ((Super, Class (4, [])))); Wildcard (Some ((Extends, Class (5, []))))]);
    Interface (13, [Wildcard (Some ((Super, Class (4, [])))); Wildcard (Some ((Super, Class (5, []))))]);
    Class (13, [Wildcard (Some ((Super, Class (4, [])))); Wildcard (Some ((Super, Class (5, []))))])
  ]

