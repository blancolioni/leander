with Ada.Directories;

with Leander.Primitives;
with Leander.Resources;

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

      --  Phase 4 integration: Leander.Create looks for a sibling .skix next
      --  to the real Prelude.hs it loads and, if fresh, primes Skit_Handle
      --  and Type_Env from it transparently (Leander.Handles.Try_Load_Image).
      --  This writes to that real resource path (not a scratch path) so a
      --  second, independent Create actually exercises that lookup.
      declare
         Image_Path : constant String :=
                        Leander.Resources.Resource_Path
                        & "/modules/Prelude.skix";
      begin
         if Ada.Directories.Exists (Image_Path) then
            Ada.Directories.Delete_File (Image_Path);
         end if;

         declare
            H1 : Leander.Handle := Leander.Create;
         begin
            H1.Dump_Module (Image_Path, "Prelude");
            H1.Close;
         end;

         declare
            H2 : Leander.Handle := Leander.Create;
         begin
            Test ("Prelude.skix: Create uses a fresh image transparently",
                  "12", H2.Evaluate ("sum (map (*2) [1,2,3])"));
            Test ("Prelude.skix: dictionary resolution via a primed image",
                  "K", H2.Evaluate ("1 == 1"));
            H2.Close;
         end;

         if Ada.Directories.Exists (Image_Path) then
            Ada.Directories.Delete_File (Image_Path);
         end if;
      end;
   end Run_Tests;

end Leander.Tests.Images;
