with Leander.Types.Trees;

package Leander.Primitives is

   function Int_Type return Leander.Types.Trees.Tree_Type;

   function List_Type return Leander.Types.Trees.Tree_Type;
   function Empty_List return Leander.Types.Trees.Tree_Type;
   function Cons return Leander.Types.Trees.Tree_Type;

   function Tuple_Name
     (Arity : Positive)
      return String;

   function Tuple_Type
     (Arity : Positive)
      return Leander.Types.Trees.Tree_Type;

   function Tuple_Con
     (Arity : Positive)
      return Leander.Types.Trees.Tree_Type;

end Leander.Primitives;
