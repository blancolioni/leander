with Leander.Prelude;

with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

with Leander.Parser.Declarations;

package body Leander.Parser.Modules is

   -----------------
   -- Load_Module --
   -----------------

   function Load_Module
     (Name : String;
      Path : String)
      return Leander.Environments.Environment
   is
      Env : Leander.Environments.Environment;
   begin
      if Name = "Prelude" then
         Env := Leander.Prelude.Prelude_Environment;
      else
         Env.Create (Name);
      end if;

      Open (Path);

      while Tok /= Tok_End_Of_File loop
         Leander.Parser.Declarations.Parse_Declaration (Env);
      end loop;

      Close;

      Leander.Environments.Annotate (Env);

      return Env;

   end Load_Module;

end Leander.Parser.Modules;
