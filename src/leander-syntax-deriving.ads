with Leander.Data_Types;
with Leander.Parser;
with Leander.Source;

package Leander.Syntax.Deriving is

   --  Each of these makes the instance a deriving clause asks for, as
   --  Haskell 2010 chapter 11 describes it, and adds it to the context's
   --  environment.  The instance's context asks the same class of each of
   --  the type's parameters.

   procedure Derive_Eq
     (Context : Leander.Parser.Parse_Context'Class;
      Loc     : Leander.Source.Source_Location;
      DT      : Leander.Data_Types.Reference);

   procedure Derive_Ord
     (Context : Leander.Parser.Parse_Context'Class;
      Loc     : Leander.Source.Source_Location;
      DT      : Leander.Data_Types.Reference);
   --  Constructors compare in the order they are declared, and the fields
   --  of one constructor from left to right.

   procedure Derive_Show
     (Context : Leander.Parser.Parse_Context'Class;
      Loc     : Leander.Source.Source_Location;
      DT      : Leander.Data_Types.Reference);
   --  A constructor applied to its fields, in parentheses above
   --  precedence 10, each field at precedence 11.

   procedure Derive_Enum
     (Context : Leander.Parser.Parse_Context'Class;
      Loc     : Leander.Source.Source_Location;
      DT      : Leander.Data_Types.Reference);
   --  Only for an enumeration: every constructor nullary.  The
   --  constructors are numbered from 0, and enumFrom and enumFromThen stop
   --  at the first or last of them.

   procedure Derive_Bounded
     (Context : Leander.Parser.Parse_Context'Class;
      Loc     : Leander.Source.Source_Location;
      DT      : Leander.Data_Types.Reference);
   --  For an enumeration, its first and last constructors; for a type of
   --  one constructor, that constructor applied to its fields' bounds.

   procedure Derive_Read
     (Context : Leander.Parser.Parse_Context'Class;
      Loc     : Leander.Source.Source_Location;
      DT      : Leander.Data_Types.Reference);
   --  Reads what a derived Show writes: a constructor alone, or applied to
   --  its fields read at precedence 11, in parentheses that are optional
   --  at precedence 10 or less and required above it.

end Leander.Syntax.Deriving;
