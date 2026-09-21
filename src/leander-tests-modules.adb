with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Leander.Calculus;
with Leander.Environment;
with Leander.Names;
with Leander.Parser;

package body Leander.Tests.Modules is

   Fixture_Dir : constant String :=
                   "./share/leander/tests/integration/modules";

   procedure Test_Repeated_Import;
   procedure Test_Resolution;
   procedure Test_Load_By_Name;
   procedure Test_Value_Names;
   procedure Test_Prelude_Exports;

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
      Test_Value_Names;
      Test_Prelude_Exports;
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

   -----------------------
   -- Test_Value_Names --
   -----------------------

   procedure Test_Value_Names is
      Context   : Leander.Parser.Parse_Context;
      Prelude   : constant Leander.Environment.Reference :=
                    Context.Load_Module
                      ("./share/leander/modules/Prelude.hs");
      Forced    : Leander.Calculus.Tree;
      Duplicate : Boolean := False;
   begin
      --  Value_Names unions the statically declared binders with whatever
      --  has been compiled into Values so far, and forcing a binding puts
      --  it in both. Dump_Module turns this list into an image's export
      --  set, where a repeat is at best a wasted entry.
      Forced := Prelude.Get_Bound_Calculus ("not");
      pragma Unreferenced (Forced);

      declare
         Names : constant Leander.Names.Name_Array := Prelude.Value_Names;
      begin
         for I in Names'Range loop
            for J in I + 1 .. Names'Last loop
               if Leander.Names.To_String (Names (I))
                 = Leander.Names.To_String (Names (J))
               then
                  Duplicate := True;
               end if;
            end loop;
         end loop;
      end;

      Test ("module: a forced binding is not named twice", not Duplicate);
   exception
      when E : others =>
         Error ("module: value names",
                Ada.Exceptions.Exception_Message (E));
   end Test_Value_Names;

   ---------------------------
   -- Test_Prelude_Exports --
   ---------------------------

   procedure Test_Prelude_Exports is
      use Ada.Strings.Unbounded;

      --  The Prelude withholds these deliberately. Everything else it
      --  declares has to be in its export list, so that the list is
      --  verifiably "the whole API" rather than whatever someone
      --  remembered to add. Adding a name here is how to withhold
      --  something on purpose; the alternative is a list that quietly
      --  drifts out of date.
      Withheld : constant String :=
                   "|mcons"            --  sequence's fold helper
                   & "|small|zero"     --  arithmetic helpers
                   & "|showUnsignedInt"  --  show helper
                   & "|bindIO|returnIO"  --  IO's Monad methods
                   & "|";

      Context : Leander.Parser.Parse_Context;
      Prelude : constant Leander.Environment.Reference :=
                  Context.Load_Module
                    ("./share/leander/modules/Prelude.hs");
      Missing : Unbounded_String;

      function Is_Withheld (Name : String) return Boolean
      is (Ada.Strings.Fixed.Index (Withheld, "|" & Name & "|") > 0);

   begin
      for N of Prelude.Declared_Names loop
         declare
            Item : constant String := Leander.Names.To_String (N);
         begin
            --  A #-prefixed name is an FFI symbol and is private by
            --  construction, never by listing.
            if Item'Length > 0
              and then Item (Item'First) /= '#'
              and then not Is_Withheld (Item)
              and then not Prelude.Is_Exported (Item)
            then
               Append (Missing, " " & Item);
            end if;
         end;
      end loop;

      if Missing = Null_Unbounded_String then
         Test ("prelude: every declaration it does not withhold is "
               & "exported", True);
      else
         Fail ("prelude: every declaration it does not withhold is "
               & "exported",
               "an export list covering all of them",
               "missing:" & To_String (Missing));
      end if;

      Test ("prelude: a withheld name really is withheld",
            not Prelude.Is_Exported ("mcons"));
      Test ("prelude: a primitive is never exported",
            not Prelude.Is_Exported ("#primIntAdd"));
      Test ("prelude: built-in syntax is always exported",
            Prelude.Is_Exported (":")
            and then Prelude.Is_Exported ("[]"));
   exception
      when E : others =>
         Error ("prelude: exports",
                Ada.Exceptions.Exception_Message (E));
   end Test_Prelude_Exports;

end Leander.Tests.Modules;
