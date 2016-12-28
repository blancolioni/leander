package body Leander.Syntax is

   ------------
   -- Create --
   ------------

   function Create
     (Node : Node_Record'Class)
      return Syntax_Tree
   is
   begin
      return Tree : Syntax_Tree_Record do
         Tree.Node := new Node_Record'Class'(Node);
      end return;
   end Create;

   -------------
   -- To_Core --
   -------------

   function To_Core
     (Syntax : Syntax_Tree)
      return Leander.Core.Trees.Tree_Type
   is
   begin
      if Syntax.Is_Expression then
         return Syntax.Get_Expression.Transform;
      else
         return Leander.Core.Trees.Empty;
      end if;
   end To_Core;

end Leander.Syntax;
