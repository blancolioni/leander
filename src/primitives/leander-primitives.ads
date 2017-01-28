with SK.Objects;

with Leander.Types.Trees;

package Leander.Primitives is

   Evaluation_Error : exception;

   function Trivial_Type return Leander.Types.Trees.Tree_Type;
   function Trivial_Con return Leander.Types.Trees.Tree_Type;

   function Char_Type return Leander.Types.Trees.Tree_Type;
   function Int_Type return Leander.Types.Trees.Tree_Type;
   function Integer_Type return Leander.Types.Trees.Tree_Type;

   function List_Type return Leander.Types.Trees.Tree_Type;
   function Empty_List return Leander.Types.Trees.Tree_Type;
   function Cons return Leander.Types.Trees.Tree_Type;

   function Map_Type return Leander.Types.Trees.Tree_Type;

   function Tuple_Name
     (Arity : Positive)
      return String;

   function Tuple_Type
     (Arity : Positive)
      return Leander.Types.Trees.Tree_Type;

   function Tuple_Con
     (Arity : Positive)
      return Leander.Types.Trees.Tree_Type;

--     function IO_Type return Leander.Types.Trees.Tree_Type;
--     function IO_Con return Leander.Types.Trees.Tree_Type;
   function World_Type return Leander.Types.Trees.Tree_Type;

   procedure Load_SK_Primitives
     (Store : in out SK.Objects.Object_Store'Class);

end Leander.Primitives;
