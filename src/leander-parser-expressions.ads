with Ada.Strings.Unbounded;

with Leander.Syntax.Patterns;

private package Leander.Parser.Expressions is

   function At_Expression return Boolean;
   function At_Pattern return Boolean;

   function Parse_Expression
     (Context : Parse_Context'Class)
      return Leander.Syntax.Expressions.Reference;

   function Parse_Atomic_Pattern
     (Context : Parse_Context'Class)
      return Leander.Syntax.Patterns.Reference;

   function Parse_Patterns
     (Context : Parse_Context'Class)
      return Leander.Syntax.Patterns.Reference_Array;

   type Associativity_Type is (Left, Right, None);
   type Priority_Range is range 0 .. 9;

   procedure Add_Fixity
     (Operator      : String;
      Associativity : Associativity_Type;
      Priority      : Priority_Range);

   procedure Set_Fixity
     (Operator      : String;
      Associativity : Associativity_Type;
      Priority      : Priority_Range);
   --  Like Add_Fixity but without the "redefinition" Warning -- for
   --  restoring a fixity table decoded from a .skix image (see
   --  Leander.Parser.Add_Fixity), a context with no active lexer session
   --  for Warning to report a source location against.

   type Fixity_Info is
      record
         Operator      : Ada.Strings.Unbounded.Unbounded_String;
         Associativity : Associativity_Type;
         Priority      : Priority_Range;
      end record;

   type Fixity_Info_Array is array (Positive range <>) of Fixity_Info;

   function All_Fixities return Fixity_Info_Array;
   --  Every operator fixity declaration registered so far (the table is a
   --  single process-global, not per-module -- see leander-handles.adb's
   --  Dump_Module/Try_Load_Image for why that's fine here: a .skix that
   --  fully covers a module also carries its fixity declarations, restored
   --  by re-calling Add_Fixity on load rather than by re-parsing them).

end Leander.Parser.Expressions;
