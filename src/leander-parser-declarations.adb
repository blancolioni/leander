with Leander.Parser.Bindings;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;
with Leander.Parser.Types;
with Leander.Syntax.Bindings;
with Leander.Syntax.Types;

package body Leander.Parser.Declarations is

   procedure Parse_Foreign_Import
     (Env : Leander.Environment.Reference);

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
         elsif Tok = Tok_Foreign then
            Scan;
            if Tok = Tok_Import then
               Scan;
               Parse_Foreign_Import (Env);
            else
               Error ("only foreign imports Supported");
               while Tok_Indent > 1 loop
                  Scan;
               end loop;
            end if;
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

   --------------------------
   -- Parse_Foreign_Import --
   --------------------------

   procedure Parse_Foreign_Import
     (Env : Leander.Environment.Reference)
   is
      Import_Type  : constant String := Tok_Text;
   begin

      if Import_Type /= "skit" then
         Error ("only skit imports are supported");
      end if;

      Scan;

      if Tok = Tok_String_Literal then
         declare
            Foreign_Name : constant String := Tok_Text;
         begin
            Scan;

            if Tok = Tok_Identifier then
               declare
                  Local_Name : constant String := Tok_Text;
               begin
                  Scan;
                  if Tok = Tok_Colon_Colon then
                     Scan;
                     declare
                        Local_Type : constant Syntax.Types.Reference :=
                                       Parser.Types.Parse_Type_Expression;
                     begin
                        Env.Foreign_Import
                          (Local_Name, Foreign_Name, Local_Type.To_Core);
                     end;
                  else
                     Error ("missing import type");
                  end if;
               end;
            else
               Error ("missing name");
            end if;
         end;
      else
         Error ("missing local name");
      end if;
   end Parse_Foreign_Import;

end Leander.Parser.Declarations;
