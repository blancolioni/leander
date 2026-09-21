with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams;

with Leander.Byte_Buffers;

with Leander.Core.Type_Classes;
with Leander.Core.Type_Classes.Serialize;
with Leander.Core.Type_Synonyms;
with Leander.Data_Types;
with Leander.Data_Types.Serialize;
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

   procedure Test_Export_Round_Trip;

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

      --  A newtype's whole point is that it has no runtime representation
      --  of its own, and that is regenerated on decode rather than stored:
      --  Data_Types.Serialize drives the Builder, which emits the identity
      --  for a newtype constructor and a Scott-encoded wrapper otherwise.
      --  So the flag itself has to survive the image, or every newtype
      --  value silently grows a wrapper when a module is loaded from one.
      declare
         NT_Path      : constant String := "test_newtype.skix";
         Saw_Newtype  : Boolean := False;
         Saw_Ordinary : Boolean := False;

         procedure On_Data_Type
           (Export_Name : String;
            Bytes       : Ada.Streams.Stream_Element_Array);

         -------------------
         -- On_Data_Type --
         -------------------

         procedure On_Data_Type
           (Export_Name : String;
            Bytes       : Ada.Streams.Stream_Element_Array)
         is
            Prefix : constant String := "datatype:";
         begin
            if Export_Name'Length > Prefix'Length
              and then Export_Name (Export_Name'First
                                     .. Export_Name'First + Prefix'Length - 1)
                       = Prefix
            then
               declare
                  DT : constant Leander.Data_Types.Reference :=
                         Leander.Data_Types.Serialize.Decode (Bytes);
               begin
                  if DT.Is_Newtype then
                     Saw_Newtype := True;
                  else
                     Saw_Ordinary := True;
                  end if;
               end;
            end if;
         end On_Data_Type;

      begin
         declare
            H : Leander.Handle := Leander.Create;
         begin
            H.Load_Module
              ("./share/leander/tests/integration/test_18_newtype.hs");
            H.Dump_Module (NT_Path, "test_18_newtype");
            H.Close;
         end;

         declare
            Hr : constant Skit.Handles.Handle :=
                   Skit.Handles.New_Handle (Core_Size => 512 * 1024);
         begin
            Leander.Primitives.Load_Primitives (Hr);
            Hr.Bind ("#error", Skit.Combinators.I);
            Skit.Handles.Images.Read (Hr, NT_Path, On_Data_Type'Access);
         end;

         Test ("newtype.skix: the newtype flag survives the image",
               Saw_Newtype);
         Test ("newtype.skix: ordinary data types are still not newtypes",
               Saw_Ordinary);

         if Ada.Directories.Exists (NT_Path) then
            Ada.Directories.Delete_File (NT_Path);
         end if;
      end;

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
                        & "modules/Prelude.skix";
         String_Id  : constant Leander.Core.Conid :=
                        Leander.Core.To_Conid ("String");
         Had_Synonym : Boolean;
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

         --  A type synonym is erased before anything runs, so an image's
         --  values hold no trace of one; it travels as its own annotation
         --  instead.  The table is global and never cleared, so proving
         --  the decode path means emptying it by hand first -- otherwise
         --  the entry left behind by the parse above would answer for it.
         Had_Synonym := Leander.Core.Type_Synonyms.Exists (String_Id);
         Leander.Core.Type_Synonyms.Clear;

         declare
            H2 : Leander.Handle := Leander.Create;
         begin
            Test ("Prelude.skix: the Prelude declares a synonym to begin with",
                  Had_Synonym);
            Test ("Prelude.skix: a type synonym is restored from the image",
                  Leander.Core.Type_Synonyms.Exists (String_Id));
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
            --  Naming String in a signature only works if the restored
            --  synonym expands; an unexpanded String would not unify with
            --  the [Char] a string literal has.
            H2.Load_Module
              ("./share/leander/tests/integration/test_19_type_synonym.hs");
            Test ("Prelude.skix: a module can name a synonym from the image",
                  "3", H2.Evaluate ("shouted"));

            H2.Load_Module
              ("./share/leander/tests/integration/test_17_default_method.hs");
            Test ("Prelude.skix: a downstream instance omitting a "
                  & "defaulted method still works",
                  "K", H2.Evaluate ("Foo 1 /= Foo 2"));

            --  Issue #66: a module that imports another, against a Prelude
            --  that came from an image rather than from source. The
            --  importing module's own names stay bare while the imported
            --  module's arrive under its name, and the Prelude's stay
            --  unprefixed -- so this is the combination where a
            --  disagreement about keys between the image and the parser
            --  would show up.
            H2.Load_Module
              ("./share/leander/tests/integration/modules/UseShapes.hs");
            Test ("Prelude.skix: a cross-module import resolves against "
                  & "an imaged Prelude",
                  "16", H2.Evaluate ("area (Square 4)"));

            H2.Load_Module
              ("./share/leander/tests/integration/modules/Qualified.hs");
            Test ("Prelude.skix: a qualified name resolves against an "
                  & "imaged Prelude",
                  "27", H2.Evaluate ("qualCircleArea"));

            H2.Close;
         end;

         if Ada.Directories.Exists (Image_Path) then
            Ada.Directories.Delete_File (Image_Path);
         end if;
      end;

      Test_Export_Round_Trip;
   end Run_Tests;

   -----------------------------
   -- Test_Export_Round_Trip --
   -----------------------------

   procedure Test_Export_Round_Trip is
      Path : constant String := "test_hidden.skix";
      Hr   : Skit.Handles.Handle;

      Saw_Meta   : Boolean := False;
      Restricted : Boolean := False;
      Has_Public : Boolean := False;
      Has_Secret : Boolean := False;

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
         package BB renames Leander.Byte_Buffers;
         C : BB.Offset := Bytes'First;
      begin
         if Export_Name /= "__leander_meta__:Hidden" then
            return;
         end if;

         Saw_Meta := True;

         --  Decoded by hand rather than through Try_Load_Image, which
         --  only ever runs for the Prelude: what is under test is the
         --  payload itself.
         if BB.Get_U8 (Bytes, C) = 2 then
            Restricted := BB.Get_U8 (Bytes, C) = 0;
            declare
               Count : constant Natural := BB.Get_U32 (Bytes, C);
            begin
               for I in 1 .. Count loop
                  declare
                     N : constant String := BB.Get_String (Bytes, C);
                  begin
                     if N = "visible" then
                        Has_Public := True;
                     elsif N = "secret" then
                        Has_Secret := True;
                     end if;
                  end;
               end loop;
            end;
         end if;
      end On_Annotation;

   begin
      declare
         H : Leander.Handle := Leander.Create;
      begin
         H.Load_Module
           ("./share/leander/tests/integration/modules/Hidden.hs");
         H.Dump_Module (Path, "Hidden");
         H.Close;
      end;

      Hr := Skit.Handles.New_Handle (Core_Size => 512 * 1024);
      --  The module's code reaches Prelude arithmetic, so the machine
      --  primitives have to be in place before the image's imports can
      --  resolve. "#error" is bound specially by Handles.Create rather
      --  than by Load_Primitives; a dummy satisfies the import, since
      --  nothing here evaluates.
      Leander.Primitives.Load_Primitives (Hr);
      Hr.Bind ("#error", Skit.Combinators.I);
      Skit.Handles.Images.Read (Hr, Path, On_Annotation'Access);

      Test ("skix: a module's meta payload is present", Saw_Meta);
      Test ("skix: an export list survives the image", Restricted);
      Test ("skix: an exported name is in the restored list", Has_Public);
      Test ("skix: an unexported name is not", not Has_Secret);

      if Ada.Directories.Exists (Path) then
         Ada.Directories.Delete_File (Path);
      end if;
   exception
      when E : others =>
         Error ("skix: export round trip",
                Ada.Exceptions.Exception_Message (E));
   end Test_Export_Round_Trip;

end Leander.Tests.Images;
