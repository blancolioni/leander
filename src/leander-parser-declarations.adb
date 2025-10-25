with Leander.Parser.Bindings;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;
with Leander.Syntax.Bindings;

package body Leander.Parser.Declarations is

   --------------------
   -- At_Declaration --
   --------------------

   function At_Declaration return Boolean is
   begin
      return At_Variable or else At_Constructor or else
        Tok <= [Tok_Class, Tok_Data, Tok_Foreign,
                Tok_Infix, Tok_Infixl, Tok_Infixr,
                Tok_Instance];
   end At_Declaration;

   ------------------------
   -- Parse_Declarations --
   ------------------------

   procedure Parse_Declarations
     (Env : Leander.Environment.Reference)
   is
      Bindings : constant Leander.Syntax.Bindings.Reference :=
                   Leander.Syntax.Bindings.Empty;
   begin
      while At_Declaration loop
         if Leander.Parser.Bindings.At_Binding then
            Leander.Parser.Bindings.Parse_Binding (Bindings);
         else
            Error ("only bindings are supported");
            Scan;
            while Tok_Indent > 1 loop
               Scan;
            end loop;
         end if;
      end loop;
      Env.Bindings (Bindings.To_Core);
   end Parse_Declarations;

end Leander.Parser.Declarations;
