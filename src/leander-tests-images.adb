with Ada.Directories;
with Ada.Streams;

with Leander.Core.Type_Classes;
with Leander.Core.Type_Classes.Serialize;
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
         Hr             : constant Skit.Handles.Handle :=
                            Skit.Handles.New_Handle (Core_Size => 512 * 1024);
         Caught         : Boolean := False;
         Decoded_Class  : Boolean := False;
         Class_Name_Ok  : Boolean := False;

         procedure On_Annotation
           (Export_Name : String;
            Bytes       : Ada.Streams.Stream_Element_Array);

         -------------------
         -- On_Annotation --
         -------------------

         procedure On_Annotation
           (Export_Name : String;
            Bytes       : Ada.Streams.Stream_Element_Array)
         is
            Prefix : constant String := "class:Prelude:";
         begin
            --  Decode one class annotation directly (bypassing Environment
            --  entirely) to prove Type_Classes.Serialize round-trips a real
            --  Prelude class declaration, independent of the full no-parse
            --  Create path exercised below.
            if not Decoded_Class
              and then Export_Name'Length > Prefix'Length
              and then Export_Name (Export_Name'First
                                     .. Export_Name'First + Prefix'Length - 1)
                       = Prefix
            then
               Decoded_Class := True;
               declare
                  Class : constant Leander.Core.Type_Classes.Reference :=
                            Leander.Core.Type_Classes.Serialize.Decode (Bytes);
               begin
                  Class_Name_Ok := Class.Methods'Length > 0;
               end;
            end if;
         end On_Annotation;

      begin
         Leander.Primitives.Load_Primitives (Hr);
         --  "#error" is bound specially by Leander.Handles.Create (not one
         --  of the generic VM primitives Load_Primitives loads); a dummy
         --  value satisfies the import here since this smoke test never
         --  evaluates anything that would actually call it.
         Hr.Bind ("#error", Skit.Combinators.I);
         begin
            Skit.Handles.Images.Read (Hr, Path, On_Annotation'Access);
         exception
            when others =>
               Caught := True;
         end;
         Test ("Prelude.skix: reads back without error", not Caught);
         Test ("Prelude.skix: a known export is bound",
               not Skit.Is_Undefined (Hr.Lookup ("sum")));
         Test ("Prelude.skix: a class-derived export is bound",
               not Skit.Is_Undefined (Hr.Lookup ("length")));
         Test ("Prelude.skix: a class annotation was found and decoded",
               Decoded_Class);
         Test ("Prelude.skix: the decoded class has methods",
               Class_Name_Ok);
      end;

      if Ada.Directories.Exists (Path) then
         Ada.Directories.Delete_File (Path);
      end if;

      --  Phase 4 integration: Leander.Create looks for a sibling .skix next
      --  to the real Prelude.hs it loads and, if it is fresh and carries the
      --  full declarative-metadata marker (see issue #65), builds Env
      --  straight from the image -- classes, instance facts, data types,
      --  and fixity all decoded rather than reparsed -- and never opens
      --  Prelude.hs at all (Leander.Handles.Create / Try_Load_Image). This
      --  writes to that real resource path (not a scratch path) so a
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
            Test ("Prelude.skix: a Maybe value pattern-matches "
                  & "(reconstructed data type)",
                  "5", H2.Evaluate ("case Just 5 of { Just x -> x; "
                                    & "Nothing -> 0 }"));
            Test ("Prelude.skix: operator fixity restored without parsing "
                  & "(2 + 3 * 4, * binds tighter)",
                  "14", H2.Evaluate ("2 + 3 * 4"));

            --  The scenario that motivated generic (dictionary-parameterized)
            --  default methods: a downstream module declares its own
            --  instance of a .skix-loaded class (Eq) and omits a method
            --  with a default ("/="). Before that change, Class_Bindings
            --  was never reconstructed from a .skix image, so this
            --  unconditionally failed to parse; now the default is resolved
            --  from the class's own precompiled generic implementation
            --  (Leander.Environment.Elaborate / Elaborate_Instance), not
            --  from source, so it works here exactly as it does when
            --  Prelude is loaded normally (see leander-tests-integration.adb).
            H2.Load_Module
              ("./share/leander/tests/integration/test_17_default_method.hs");
            Test ("Prelude.skix: a downstream instance omitting a "
                  & "defaulted method still works",
                  "K", H2.Evaluate ("Foo 1 /= Foo 2"));
            H2.Close;
         end;

         if Ada.Directories.Exists (Image_Path) then
            Ada.Directories.Delete_File (Image_Path);
         end if;
      end;
   end Run_Tests;

end Leander.Tests.Images;
