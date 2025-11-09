with Leander.Environment.Prelude;

with Leander.Parser.Declarations;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

package body Leander.Parser.Modules is

   ------------------
   -- Parse_Module --
   ------------------

   function Parse_Module
     (Name : String)
      return Leander.Environment.Reference
   is
      Env : constant Leander.Environment.Reference :=
              (if Name = "Prelude"
               then Leander.Environment.Prelude.Create
               else Leander.Environment.New_Environment (Name));
   begin
      Expect (Tok_Module, [Tok_Identifier]);

      if Tok = Tok_Identifier then
         if Tok_Text /= Name then
            Error ("expected module " & Name & "; found " & Tok_Text);
         end if;
         Scan;
      else
         Error ("expected module name");
      end if;

      Expect (Tok_Where,
              [Tok_Identifier, Tok_Data, Tok_Type, Tok_Newtype,
               Tok_Class, Tok_Instance]);

      begin
         Declarations.Parse_Declarations (Env);

         Expect (Tok_End_Of_File, Tok_End_Of_File);

      exception
         when Parse_Error =>
            null;
      end;

      return Env;
   end Parse_Module;

end Leander.Parser.Modules;
