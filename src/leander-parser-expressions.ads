with Leander.Syntax.Patterns;
private package Leander.Parser.Expressions is

   function At_Expression return Boolean;
   function At_Pattern return Boolean;

   function Parse_Expression
     (Context : Parse_Context'Class)
      return Leander.Syntax.Expressions.Reference;

   function Parse_Patterns
     (Context : Parse_Context'Class)
      return Leander.Syntax.Patterns.Reference_Array;

   type Associativity_Type is (Left, Right, None);
   type Priority_Range is range 0 .. 9;

   procedure Add_Fixity
     (Operator      : String;
      Associativity : Associativity_Type;
      Priority      : Priority_Range);

end Leander.Parser.Expressions;
