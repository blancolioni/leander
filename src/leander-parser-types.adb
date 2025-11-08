with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

package body Leander.Parser.Types is

   function At_Atomic_Type return Boolean
   is (At_Name or else Tok <= [Tok_Left_Bracket, Tok_Left_Paren]);

   -----------------------
   -- Parse_Atomic_Type --
   -----------------------

   function Parse_Atomic_Type
     return Leander.Syntax.Types.Reference
   is
      use Leander.Syntax.Types;
      Loc    : constant Leander.Source.Source_Location :=
                 Current_Source_Location;
   begin
      if At_Variable then
         return T : constant Reference := Variable (Loc, Tok_Text) do
            Scan;
         end return;
      elsif At_Constructor then
         return T : constant Reference := Constructor (Loc, Tok_Text) do
            Scan;
         end return;
      elsif Tok = Tok_Left_Bracket then
         Scan;
         if Tok = Tok_Right_Bracket then
            return T : constant Reference := Constructor (Loc, "[]") do
               Scan;
            end return;
         else
            declare
               Inner : constant Reference := Parse_Type_Expression;
            begin
               Expect (Tok_Right_Bracket, [Tok_Right_Arrow, Tok_Identifier]);
               return Application (Loc, Constructor (Loc, "[]"), Inner);
            end;
         end if;
      else
         Error ("expected an atomic type at " & Tok'Image);
         raise Parse_Error;
      end if;

   end Parse_Atomic_Type;

   ---------------------------
   -- Parse_Type_Expression --
   ---------------------------

   function Parse_Type_Expression
     return Leander.Syntax.Types.Reference
   is
      use Leander.Syntax.Types;
      Indent : constant Positive := Tok_Indent;
      Loc    : constant Leander.Source.Source_Location :=
                 Current_Source_Location;
      Expr : Reference := Parse_Atomic_Type;
   begin

      while Tok_Indent > Indent
        and then At_Atomic_Type
      loop
         declare
            Right : constant Reference := Parse_Atomic_Type;
         begin
            Expr := Application (Loc, Expr, Right);
         end;
      end loop;

      if Tok = Tok_Right_Arrow then
         Scan;
         declare
            Target : constant Reference := Parse_Type_Expression;
         begin
            Expr :=
              Application
                (Loc,
                 Application
                   (Loc,
                    Constructor (Loc, "(->)"), Expr),
               Target);
         end;
      end if;

      return Expr;
   end Parse_Type_Expression;

end Leander.Parser.Types;
