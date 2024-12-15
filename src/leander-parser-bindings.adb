
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Parser.Expressions;
with Leander.Syntax.Patterns;

package body Leander.Parser.Bindings is

   function At_Binding return Boolean
   is (Tok = Tok_Identifier);

   -------------------
   -- Parse_Binding --
   -------------------

   procedure Parse_Binding
     (To : Leander.Syntax.Bindings.Reference)
   is
      Pat : constant Leander.Syntax.Patterns.Reference :=
              Leander.Syntax.Patterns.Variable (Current_Source_Location,
                                                Tok_Text);
   begin
      Scan;
      Expect (Tok_Equal, []);
      declare
         Expr : constant Leander.Syntax.Expressions.Reference :=
                  Leander.Parser.Expressions.Parse_Expression;
      begin
         To.Add_Binding (Pat, Expr);
      end;
   end Parse_Binding;

end Leander.Parser.Bindings;
