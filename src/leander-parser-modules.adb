with Leander.Environment.Prelude;

with Leander.Parser.Declarations;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

package body Leander.Parser.Modules is

   ------------------
   -- Parse_Module --
   ------------------

   function Parse_Module
     (Context     : in out Parse_Context'Class;
      Name        : String;
      Prelude_Env : Leander.Environment.Reference := null)
      return Leander.Environment.Reference
   is
      use type Leander.Environment.Reference;
      Env : constant Leander.Environment.Reference :=
              (if Name = "Prelude"
               then Leander.Environment.Prelude.Create
               else Leander.Environment.New_Environment (Name));
   begin
      Context.New_Environment (Env);
      if Prelude_Env /= null then
         Env.Import (Prelude_Env);
      end if;

      Expect (Tok_Where,
              [Tok_Identifier, Tok_Data, Tok_Type, Tok_Newtype,
               Tok_Class, Tok_Instance]);

      begin
         Declarations.Parse_Declarations (Context);

         Expect (Tok_End_Of_File, Tok_End_Of_File);

      exception
         when Parse_Error =>
            null;
      end;

      return Env;
   end Parse_Module;

   ------------------------
   -- Scan_Module_Header --
   ------------------------

   function Scan_Module_Header return String is
   begin
      Expect (Tok_Module, [Tok_Identifier]);

      if Tok = Tok_Identifier
        and then Is_Alphanumeric_Identifier (Tok_Text)
      then
         return Scan_Dotted_Name;
      else
         Error ("expected module name");
         return "";
      end if;
   end Scan_Module_Header;

end Leander.Parser.Modules;
