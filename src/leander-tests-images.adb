with Ada.Directories;

with Leander.Primitives;

with Skit;
with Skit.Combinators;
with Skit.Handles;
with Skit.Handles.Images;

package body Leander.Tests.Images is

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      Path : constant String := "test_prelude.skix";
   begin
      declare
         H : Leander.Handle := Leander.Create;
      begin
         H.Dump_Module (Path, "Prelude");
         H.Close;
      end;

      declare
         Hr     : constant Skit.Handles.Handle :=
                    Skit.Handles.New_Handle (Core_Size => 512 * 1024);
         Caught : Boolean := False;
      begin
         Leander.Primitives.Load_Primitives (Hr);
         --  "#error" is bound specially by Leander.Handles.Create (not one
         --  of the generic VM primitives Load_Primitives loads); a dummy
         --  value satisfies the import here since this smoke test never
         --  evaluates anything that would actually call it.
         Hr.Bind ("#error", Skit.Combinators.I);
         begin
            Skit.Handles.Images.Read (Hr, Path);
         exception
            when others =>
               Caught := True;
         end;
         Test ("Prelude.skix: reads back without error", not Caught);
         Test ("Prelude.skix: a known export is bound",
               not Skit.Is_Undefined (Hr.Lookup ("sum")));
         Test ("Prelude.skix: a class-derived export is bound",
               not Skit.Is_Undefined (Hr.Lookup ("length")));
      end;

      if Ada.Directories.Exists (Path) then
         Ada.Directories.Delete_File (Path);
      end if;
   end Run_Tests;

end Leander.Tests.Images;
