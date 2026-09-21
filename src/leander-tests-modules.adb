with Ada.Exceptions;

with Leander.Environment;
with Leander.Parser;

package body Leander.Tests.Modules is

   Fixture_Dir : constant String :=
                   "./share/leander/tests/integration/modules";

   procedure Test_Repeated_Import;
   procedure Test_Resolution;
   procedure Test_Load_By_Name;

   function Ends_With (Value, Suffix : String) return Boolean
   is (Value'Length >= Suffix'Length
       and then Value (Value'Last - Suffix'Length + 1 .. Value'Last)
                  = Suffix);

   function Normalized (Path : String) return String;
   --  Full_Name hands back platform separators; the tests only care which
   --  file was found.

   ---------------
   -- Normalized --
   ---------------

   function Normalized (Path : String) return String is
      Result : String := Path;
   begin
      for Ch of Result loop
         if Ch = '\' then
            Ch := '/';
         end if;
      end loop;
      return Result;
   end Normalized;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
   begin
      Test_Repeated_Import;
      Test_Resolution;
      Test_Load_By_Name;
   end Run_Tests;

   ------------------------
   -- Test_Load_By_Name --
   ------------------------

   procedure Test_Load_By_Name is
      use type Leander.Environment.Reference;
      Context : Leander.Parser.Parse_Context;
      Prelude : constant Leander.Environment.Reference :=
                  Context.Load_Module
                    ("./share/leander/modules/Prelude.hs");
      --  Whatever the context was pointing at before a load has to be
      --  what it points at afterwards. Note this is null here rather than
      --  Prelude: Prelude is already in the module cache by now, so the
      --  Load_Module above returned it without parsing, and only a parse
      --  sets the context environment.
      Before  : constant Leander.Environment.Reference :=
                  Context.Environment;
   begin
      Test ("module: the Prelude loads before anything else",
            Prelude /= null);

      declare
         Listed : constant Leander.Environment.Reference :=
                    Context.Load_Module_By_Name
                      ("Data.List", From_Dir => Fixture_Dir);
      begin
         Test ("module: a dotted module name loads from a subdirectory",
               Listed /= null
               and then Listed.Name = "Data.List");

         --  The name map, not the file name, is what says a module is
         --  already loaded: List.hs's base name is "List", so a second
         --  request has to match on "Data.List" to come back identical.
         Test ("module: loading the same module twice returns one "
               & "environment",
               Listed = Context.Load_Module_By_Name
                 ("Data.List", From_Dir => Fixture_Dir));
      end;

      Test ("module: an unknown module reports rather than raises",
            Context.Load_Module_By_Name
              ("No.Such.Module", From_Dir => Fixture_Dir) = null);

      Test ("module: a load that comes out of the cache leaves the "
            & "environment alone",
            Context.Environment = Before);

      --  Load_Module_By_Name is written to be callable mid-parse, which
      --  means leaving the caller's environment alone -- Parse_Module
      --  points the context at the module it is building, so without the
      --  restore an importer would carry on against the imported module.
      --  Fixture is loaded nowhere else, so this really does parse.
      Context.New_Environment (Prelude);
      declare
         Fixture : constant Leander.Environment.Reference :=
                     Context.Load_Module_By_Name
                       ("Fixture", From_Dir => Fixture_Dir);
      begin
         Test ("module: a module beside the importer is found",
               Fixture /= null);
         Test ("module: a parse started mid-parse restores the caller",
               Context.Environment = Prelude);
      end;
   exception
      when E : others =>
         Error ("module: loading by name",
                Ada.Exceptions.Exception_Message (E));
   end Test_Load_By_Name;

   ---------------------------
   -- Test_Repeated_Import --
   ---------------------------

   procedure Test_Repeated_Import is
      Context : Leander.Parser.Parse_Context;
      Prelude : constant Leander.Environment.Reference :=
                  Context.Load_Module
                    ("./share/leander/modules/Prelude.hs");
      A       : constant Leander.Environment.Reference :=
                  Leander.Environment.New_Environment ("A");
      B       : constant Leander.Environment.Reference :=
                  Leander.Environment.New_Environment ("B");
      M       : constant Leander.Environment.Reference :=
                  Leander.Environment.New_Environment ("M");
   begin
      --  The shape a module with two import declarations will have once
      --  issue #66 lands: Prelude is imported implicitly, then each named
      --  import brings a chain that already contains Prelude's links. The
      --  second one re-flattens those names into a map that already holds
      --  them, which is what Type_Env.Compose's duplicate guard exists for.
      A.Import (Prelude);
      B.Import (Prelude);

      M.Import (Prelude);
      M.Import (A);
      M.Import (B);

      Test ("import: a module may import two modules that share a Prelude",
            True);
   exception
      when E : others =>
         Error ("import: a module may import two modules that share a Prelude",
                Ada.Exceptions.Exception_Message (E));
   end Test_Repeated_Import;

   ---------------------
   -- Test_Resolution --
   ---------------------

   procedure Test_Resolution is
   begin
      Test ("module: a dotted name resolves to a subdirectory",
            Ends_With
              (Normalized
                 (Leander.Parser.Resolve_Module_Path
                    ("Data.List", From_Dir => Fixture_Dir)),
               "/modules/Data/List.hs"));

      Test ("module: the installed module directory is searched last",
            Ends_With
              (Normalized
                 (Leander.Parser.Resolve_Module_Path
                    ("Prelude", From_Dir => Fixture_Dir)),
               "/modules/Prelude.hs"));

      Test ("module: a module with no source file resolves to nothing",
            Leander.Parser.Resolve_Module_Path
              ("No.Such.Module", From_Dir => Fixture_Dir) = "");

      --  An include path is searched after the importing file's own
      --  directory but before the installed one, so a module that is
      --  nowhere else becomes findable with no From_Dir at all.
      Test ("module: an include path is not searched before it is added",
            Leander.Parser.Resolve_Module_Path
              ("Data.List", From_Dir => "") = "");

      Leander.Parser.Add_Include_Path (Fixture_Dir);

      Test ("module: an include path is searched when no directory is given",
            Ends_With
              (Normalized
                 (Leander.Parser.Resolve_Module_Path
                    ("Data.List", From_Dir => "")),
               "/modules/Data/List.hs"));
   exception
      when E : others =>
         Error ("module: resolution",
                Ada.Exceptions.Exception_Message (E));
   end Test_Resolution;

end Leander.Tests.Modules;
