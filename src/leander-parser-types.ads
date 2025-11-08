with Leander.Syntax.Types;

package Leander.Parser.Types is

   function At_Atomic_Type return Boolean;

   function Parse_Atomic_Type
     return Leander.Syntax.Types.Reference;

   function Parse_Type_Expression
     return Leander.Syntax.Types.Reference;

end Leander.Parser.Types;
