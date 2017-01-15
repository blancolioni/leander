private package Leander.Parser.Expressions is

   function At_Expression return Boolean;
   function At_Pattern return Boolean renames At_Expression;

   function Parse_Expression return Leander.Core.Trees.Tree_Type;
   function Parse_Atomic_Pattern return Leander.Core.Trees.Tree_Type;
   function Parse_Pattern return Leander.Core.Trees.Tree_Type;

end Leander.Parser.Expressions;
