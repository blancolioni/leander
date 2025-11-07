with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Parser.Expressions;
with Leander.Parser.Types;

with Leander.Syntax.Patterns;
with Leander.Syntax.Types;

package body Leander.Parser.Bindings is

   function At_Binding return Boolean
   is (At_Variable);

   -------------------
   -- Parse_Binding --
   -------------------

   procedure Parse_Binding
     (To : Leander.Syntax.Bindings.Reference)
   is
      Loc  : constant Source.Source_Location := Current_Source_Location;
      Name : constant String := Scan_Identifier;
      Pats : constant Leander.Syntax.Patterns.Reference_Array :=
               Expressions.Parse_Patterns;
   begin
      if Tok = Tok_Colon_Colon then
         Scan;
         declare
            Expr : constant Leander.Syntax.Types.Reference :=
                     Leander.Parser.Types.Parse_Type_Expression;
         begin
            To.Add_Type (Loc, Name, Expr);
         end;
      elsif Tok = Tok_Equal then
         Scan;
         declare
            Expr : constant Leander.Syntax.Expressions.Reference :=
                     Leander.Parser.Expressions.Parse_Expression;
         begin
            To.Add_Binding (Loc, Name, Pats, Expr);
         end;
      else
         Error ("expected '::' or '=' at " & Tok'Image);
         raise Parse_Error;
      end if;
   end Parse_Binding;

end Leander.Parser.Bindings;
