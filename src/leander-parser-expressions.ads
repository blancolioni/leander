private package Leander.Parser.Expressions is

   function At_Expression return Boolean;

   function Parse_Expression return Leander.Syntax.Expressions.Reference;

   type Associativity_Type is (Left, Right, None);
   type Priority_Range is range 0 .. 9;

   procedure Add_Fixity
     (Operator      : String;
      Associativity : Associativity_Type;
      Priority      : Priority_Range);

end Leander.Parser.Expressions;
