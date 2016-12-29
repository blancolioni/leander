with Leander.Syntax;

private package Leander.Parser.Expressions is

   function At_Expression return Boolean;
   function At_Pattern return Boolean renames At_Expression;

   function Parse_Expression return Leander.Syntax.Syntax_Tree;
   function Parse_Atomic_Pattern return Leander.Syntax.Syntax_Tree;
   function Parse_Pattern return Leander.Syntax.Syntax_Tree;

end Leander.Parser.Expressions;
