with Leander.Core.Predicates;
with Leander.Syntax.Qualified_Types;
with Leander.Syntax.Types;

package Leander.Parser.Types is

   function At_Atomic_Type return Boolean;

   function Parse_Atomic_Type
     (Context : Parse_Context'Class)
     return Leander.Syntax.Types.Reference;

   function Parse_Type_Expression
     (Context : Parse_Context'Class)
      return Leander.Syntax.Types.Reference;

   function Parse_Qualified_Type_Expression
     (Context : Parse_Context'Class)
      return Leander.Syntax.Qualified_Types.Reference;

   function Parse_Constraint
     (Context : Parse_Context'Class)
      return Leander.Core.Predicates.Predicate_Array;

end Leander.Parser.Types;
