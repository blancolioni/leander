with Ada.Strings.Unbounded;

with Leander.Parser.Tokens;
with Leander.Syntax.Patterns;

private package Leander.Parser.Expressions is

   function At_Expression return Boolean;
   function At_Pattern return Boolean;

   function Parse_Expression
     (Context : Parse_Context'Class)
      return Leander.Syntax.Expressions.Reference;

   type Guarded_RHS is
      record
         Expr        : Leander.Syntax.Expressions.Reference;
         Fallthrough : Ada.Strings.Unbounded.Unbounded_String;
      end record;
   --  Expr is the right-hand side.  Fallthrough names the free variable the
   --  guard chain falls out to when every guard fails; it is empty when the
   --  right-hand side is unguarded and so cannot fail.  The parser cannot
   --  know what a failed guard should do -- that depends on whether another
   --  equation follows -- so it leaves the name free and the owner of the
   --  equation list binds it: see Leander.Syntax.Bindings.Guards.

   function Parse_Guarded_RHS
     (Context   : Parse_Context'Class;
      Separator : Leander.Parser.Tokens.Token)
      return Guarded_RHS;
   --  Parse one right-hand side: either "Separator expr", or a sequence of
   --  "| g1, g2 Separator expr" guard arms.  Separator is Tok_Equal for a
   --  function equation and Tok_Right_Arrow for a case alternative.
   --
   --  Arms are consumed greedily, across line breaks.  They have to be: the
   --  layout-driven sequence drivers stop at any token that cannot start an
   --  element, so a continuation line beginning with '|' would otherwise
   --  close the enclosing declaration or alternative list rather than
   --  continue this right-hand side.  Nothing else can begin with '|' here,
   --  so being greedy is safe.

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
