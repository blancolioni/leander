package Leander.Trees is

   type Tree_Interface is interface and Show_Interface;

   function Is_Empty (Tree : Tree_Interface) return Boolean is abstract;
   function Is_Leaf (Tree : Tree_Interface) return Boolean is abstract;
   function Is_Application (Tree : Tree_Interface) return Boolean is abstract;

   function Empty return Tree_Interface
     is abstract
     with Post'Class => Tree_Interface'Class (Empty'Result).Is_Empty;

   function Left (Tree : Tree_Interface) return Tree_Interface
                  is abstract
     with Pre'Class => Tree_Interface'Class (Tree).Is_Application;

   function Right (Tree : Tree_Interface) return Tree_Interface
                   is abstract
     with Pre'Class => Tree_Interface'Class (Tree).Is_Application;

   function Apply (Left, Right : Tree_Interface) return Tree_Interface
                   is abstract
     with Post'Class => Tree_Interface'Class (Apply'Result).Is_Application;

end Leander.Trees;
