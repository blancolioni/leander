with Ada.Characters.Handling;

with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Primitives;

with Leander.Syntax.Expressions;

package body Leander.Parser.Expressions is

   function At_Atomic_Expression return Boolean;
   function At_Atomic_Pattern return Boolean renames At_Atomic_Expression;

   function Parse_Atomic_Expression return Leander.Syntax.Syntax_Tree;
   function Parse_Case_Expression return Leander.Syntax.Syntax_Tree;

   function Parse_Atomic_Pattern return Leander.Syntax.Syntax_Tree
     renames Parse_Atomic_Expression;

   function Parse_Pattern return Leander.Syntax.Syntax_Tree
     renames Parse_Expression;

   --------------------------
   -- At_Atomic_Expression --
   --------------------------

   function At_Atomic_Expression return Boolean is
      use Leander.Parser.Lexical.Set_Of_Tokens;
   begin
      return Tok <= +(Tok_Identifier, Tok_Integer_Literal,
                      Tok_Character_Literal, Tok_Float_Literal,
                      Tok_String_Literal,
                      Tok_Left_Paren, Tok_Left_Bracket);
   end At_Atomic_Expression;

   -------------------
   -- At_Expression --
   -------------------

   function At_Expression return Boolean is
      use Leander.Parser.Lexical.Set_Of_Tokens;
   begin
      return At_Atomic_Expression
        or else Tok <= +Tok_Lambda;
   end At_Expression;

   -----------------------------
   -- Parse_Atomic_Expression --
   -----------------------------

   function Parse_Atomic_Expression return Leander.Syntax.Syntax_Tree is
      Current : constant Leander.Source.Source_Reference :=
                  Current_Source_Reference;
   begin
      if Tok = Tok_Identifier then
         declare
            Name : constant String := Tok_Text;
         begin
            Scan;
            if Ada.Characters.Handling.Is_Upper (Name (Name'First)) then
               return Leander.Syntax.Expressions.Constructor
                 (Current, Name);
            else
               return Leander.Syntax.Expressions.Variable (Current, Name);
            end if;
         end;
      elsif Tok = Tok_Integer_Literal then
         declare
            Literal : constant String := Tok_Text;
         begin
            Scan;
            return Leander.Syntax.Expressions.Literal
              (Current, Literal, Leander.Primitives.Int_Type);
         end;
      elsif Tok = Tok_Left_Paren then
         Scan;
         declare
            Expr : constant Leander.Syntax.Syntax_Tree := Parse_Expression;
         begin
            if Tok = Tok_Right_Paren then
               Scan;
            else
               Error ("missing ')'");
            end if;
            return Expr;
         end;
      elsif Tok = Tok_Left_Bracket then
         if Next_Tok = Tok_Right_Bracket then
            declare
               Expr : constant Leander.Syntax.Syntax_Tree :=
                        Leander.Syntax.Expressions.Constructor
                          (Current, "[]");
            begin
               Scan;
               Scan;
               return Expr;
            end;
         else
            Error ("lists not implemented yet");
            while Tok /= Tok_Right_Bracket
              and then Tok /= Tok_End_Of_File
            loop
               Scan;
            end loop;
            if Tok = Tok_Right_Bracket then
               Scan;
            end if;
            return Leander.Syntax.Expressions.Variable (Current, "_");
         end if;
      else
         Error ("expected atomic expression");
         Scan;
         return Leander.Syntax.Expressions.Variable (Current, "_");
      end if;
   end Parse_Atomic_Expression;

   ---------------------------
   -- Parse_Case_Expression --
   ---------------------------

   function Parse_Case_Expression return Leander.Syntax.Syntax_Tree is
      Current : constant Leander.Source.Source_Reference :=
                  Current_Source_Reference;
   begin
      pragma Assert (Tok = Tok_Case);
      Scan;

      declare
         E : constant Leander.Syntax.Syntax_Tree := Parse_Expression;
         Result : constant Leander.Syntax.Syntax_Tree :=
                    Leander.Syntax.Expressions.Case_Expression
                      (Current, E);
      begin
         if Tok = Tok_Of then
            Scan;
         else
            Error ("missing 'of'");
         end if;

         declare
            Indent : constant Positive := Tok_Indent;
         begin
            while Tok_Indent >= Indent
              and then At_Atomic_Pattern
            loop
               declare
                  Pat : constant Leander.Syntax.Syntax_Tree :=
                          Leander.Parser.Expressions.Parse_Pattern;
               begin
                  if Tok = Tok_Right_Arrow then
                     Scan;
                  else
                     Error ("missing '->'");
                  end if;

                  declare
                     Exp : constant Leander.Syntax.Syntax_Tree :=
                             Parse_Expression;
                  begin
                     Leander.Syntax.Expressions.Add_Case_Alternate
                       (Case_Expr  => Result,
                        Pattern    => Pat,
                        Expression => Exp);
                  end;
               end;
            end loop;
         end;

         return Result;
      end;
   end Parse_Case_Expression;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression return Leander.Syntax.Syntax_Tree is
      Current : constant Leander.Source.Source_Reference :=
                  Current_Source_Reference;
   begin
      if At_Atomic_Expression then
         declare
            Indent  : constant Positive := Tok_Indent;
            Expr    : Leander.Syntax.Syntax_Tree :=
                        Parse_Atomic_Expression;
         begin
            while At_Atomic_Expression
              and then Tok_Indent > Indent
            loop
               Expr :=
                 Leander.Syntax.Expressions.Apply
                   (Current, Expr, Parse_Atomic_Expression);
            end loop;
            return Expr;
         end;
      elsif Tok = Tok_Lambda then
         Scan;
         declare
            Name : constant String :=
                     (if At_Variable then Tok_Text else "_");
         begin
            if not At_Variable then
               Error ("missing lambda variable");
            else
               Scan;
            end if;

            if Tok = Tok_Right_Arrow then
               Scan;
            else
               Error ("missing ->");
            end if;

            declare
               Expr : constant Leander.Syntax.Syntax_Tree :=
                        Parse_Expression;
            begin
               return Leander.Syntax.Expressions.Lambda (Current, Name, Expr);
            end;
         end;
      elsif Tok = Tok_Case then
         return Parse_Case_Expression;
      else
         raise Constraint_Error with
           "expected to be at an expression";
      end if;

   end Parse_Expression;

end Leander.Parser.Expressions;
