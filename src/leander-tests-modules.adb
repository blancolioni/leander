with Ada.Exceptions;

with Leander.Environment;
with Leander.Parser;

package body Leander.Tests.Modules is

   procedure Test_Repeated_Import;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
   begin
      Test_Repeated_Import;
   end Run_Tests;

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

end Leander.Tests.Modules;
