private package Leander.Parser.Expressions is

   function At_Expression return Boolean;
   function At_Pattern return Boolean renames At_Expression;

   function Parse_Expression return Leander.Core.Trees.Tree_Type;
   function Parse_Atomic_Pattern return Leander.Core.Trees.Tree_Type;
   function Parse_Pattern return Leander.Core.Trees.Tree_Type;
   function Parse_Guarded_Expression return Leander.Core.Trees.Tree_Type;

   type Associativity_Type is (Left, Right, None);
   type Priority_Range is range 0 .. 9;

   procedure Add_Fixity
     (Operator      : String;
      Associativity : Associativity_Type;
      Priority      : Priority_Range);

end Leander.Parser.Expressions;
