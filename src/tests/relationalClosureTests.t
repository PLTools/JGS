  $ ./relationalClosureTests.exe
  Class A: 4
  Class B extends A: 5
  Class C extends B: 6
  Class A1: 7
  ? <-< A: 
  [
    Var {id=_.326, index=_.327, upb=Class (4, []), lwb=None};
    Null;
    Intersect ([Class (4, []) | _.195]);
    Class (4, []);
    Intersect ([_.197 [=/= Class (4, [])]; Class (4, []) | _.973]);
    Var {id=_.331, index=_.332, upb=Null, lwb=None};
    Intersect ([_.197 [=/= Class (4, [])]; _.975 [=/= Class (4, [])]; Class (4, []) | _.1511]);
    Intersect ([_.197 [=/= Class (4, [])]; _.975 [=/= Class (4, [])]; _.1513 [=/= Class (4, [])]; Class (4, []) | _.1783]);
    Intersect ([_.197 [=/= Class (4, [])]; _.975 [=/= Class (4, [])]; _.1513 [=/= Class (4, [])]; _.1785 [=/= Class (4, [])]; Class (4, []) | _.2070]);
    Intersect ([_.197 [=/= Class (4, [])]; _.975 [=/= Class (4, [])]; _.1513 [=/= Class (4, [])]; _.1785 [=/= Class (4, [])]; _.2072 [=/= Class (4, [])]; Class (4, []) | _.2337])
  ]
  
  ? <-< A (without intersects vars and null): 
  [
    Class (4, []);
    Class (5, []);
    Class (5, []);
    Class (5, []);
    Class (5, []);
    Class (6, []);
    Class (5, []);
    Class (5, []);
    Class (6, []);
    Class (5, [])
  ]
  
  C <-< A: 
  [
    Class (6, []);
    Class (6, []);
    Class (6, []);
    Class (6, []);
    Class (6, []);
    Class (6, []);
    Class (6, []);
    Class (6, []);
    Class (6, []);
    Class (6, [])
  ]
  
  B <-1-< A: 
  [
    [Class (4, []); Class (5, [])]
  ]
  
  B <-2-< A: 
  [
    [Class (4, []); Var {id=_.13073, index=_.13074, upb=Class (4, []), lwb=Some (Class (5, []))}; Class (5, [])];
    [Class (4, []); Var {id=_.13073, index=_.13074, upb=Class (4, []), lwb=Some (Class (5, []))}; Class (5, [])];
    [Class (4, []); Var {id=_.13073, index=_.13074, upb=Class (4, []), lwb=Some (Class (5, []))}; Class (5, [])];
    [Class (4, []); Var {id=_.13073, index=_.13074, upb=Class (4, []), lwb=Some (Class (5, []))}; Class (5, [])];
    [Class (4, []); Var {id=_.13073, index=_.13074, upb=Class (4, []), lwb=Some (Class (5, []))}; Class (5, [])];
    [Class (4, []); Var {id=_.13073, index=_.13074, upb=Class (4, []), lwb=Some (Class (5, []))}; Class (5, [])];
    [Class (4, []); Var {id=_.13073, index=_.13074, upb=Class (4, []), lwb=Some (Class (5, []))}; Class (5, [])];
    [Class (4, []); Var {id=_.13073, index=_.13074, upb=Class (4, []), lwb=Some (Class (5, []))}; Class (5, [])];
    [Class (4, []); Var {id=_.13073, index=_.13074, upb=Class (4, []), lwb=Some (Class (5, []))}; Class (5, [])];
    [Class (4, []); Var {id=_.13073, index=_.13074, upb=Class (4, []), lwb=Some (Class (5, []))}; Class (5, [])]
  ]
  
  C <-1-< A: 
  [
    
  ]
  
  C <-2-< A: 
  [
    [Class (4, []); Class (5, []); Class (6, [])]
  ]
  
