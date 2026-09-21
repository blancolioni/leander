with Leander.Core;
with Leander.Data_Types;
with Leander.Environment.Prelude;
with Leander.Source;

with Leander.Names;
with Leander.Scopes;
with Leander.Syntax.Modules;

with Leander.Parser.Declarations;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;

package body Leander.Parser.Modules is

   --  "qualified", "hiding" and "as" are contextual in Haskell and are
   --  deliberately not tokens. Leander.Parser.Lexical's Keywords string is
   --  positional against First_Keyword, so adding one shifts every later
   --  keyword; matching on Tok_Text follows the precedent set by the
   --  "skit" in a foreign import declaration.

   function At_Soft_Keyword
     (Word      : String;
      Followed  : Token)
      return Boolean
   is (Tok = Tok_Identifier
       and then Tok_Text = Word
       and then Next_Tok (1) = Followed);
   --  A contextual keyword only counts as one when what follows it fits,
   --  which is how Haskell keeps "import qualified" working for a module
   --  that happens to be called "qualified".

   procedure Parse_Export_List
     (Exports : in out Leander.Syntax.Modules.Export_List);

   procedure Apply_Exports
     (Env      : Leander.Environment.Reference;
      Exports  : Leander.Syntax.Modules.Export_List;
      Location : Leander.Source.Source_Location);

   procedure Parse_Import
     (Context  : in out Parse_Context'Class;
      Env      : Leander.Environment.Reference;
      From_Dir : String);

   procedure Do_Import
     (Context  : in out Parse_Context'Class;
      Env      : Leander.Environment.Reference;
      Name     : String;
      Alias    : String;
      From_Dir : String;
      Import   : Leander.Syntax.Modules.Import_Declaration);
   --  Load Name and bring it into Env. What the import declaration
   --  restricts is what may be written, so the decision it carries is
   --  passed to Environment.Import, which acts on it by choosing which
   --  keys a name arrives under rather than by removing anything.

   -------------------
   -- Apply_Exports --
   -------------------

   procedure Apply_Exports
     (Env      : Leander.Environment.Reference;
      Exports  : Leander.Syntax.Modules.Export_List;
      Location : Leander.Source.Source_Location)
   is
      procedure Export (Item : String);

      ------------
      -- Export --
      ------------

      procedure Export (Item : String) is
      begin
         if Env.Declares (Item) then
            Env.Add_Export (Item);
         else
            Report (Location,
                    "module " & Env.Name & " exports " & Item
                    & ", which it does not declare");
         end if;
      end Export;

   begin
      Env.Start_Exports;

      for I in 1 .. Exports.Count loop
         declare
            Item : constant String := Exports.Name (I);
         begin
            Export (Item);

            if Exports.Exports_Constructors (I) then
               --  "T(..)" means a data type's constructors or a class's
               --  methods, depending on what T turns out to be, and
               --  neither can be named until the declarations are in.
               if Env.Exists (Leander.Names.To_Leander_Name (Item),
                              Leander.Environment.Type_Constructor)
               then
                  declare
                     DT : constant Leander.Data_Types.Reference :=
                            Env.Data_Type (Leander.Core.To_Conid (Item));
                  begin
                     for J in 1 .. DT.Constructor_Count loop
                        Export
                          (Leander.Core.To_String (DT.Constructor_Name (J)));
                     end loop;
                  end;
               elsif Env.Exists (Leander.Names.To_Leander_Name (Item),
                                 Leander.Environment.Class_Binding)
               then
                  for M of Env.Get_Class (Leander.Core.To_Conid (Item))
                             .Methods
                  loop
                     Export (Leander.Core.To_String (M));
                  end loop;
               end if;
            else
               for J in 1 .. Exports.Constructor_Count (I) loop
                  Export (Exports.Constructor (I, J));
               end loop;
            end if;
         end;
      end loop;
   end Apply_Exports;

   ---------------
   -- Do_Import --
   ---------------

   procedure Do_Import
     (Context  : in out Parse_Context'Class;
      Env      : Leander.Environment.Reference;
      Name     : String;
      Alias    : String;
      From_Dir : String;
      Import   : Leander.Syntax.Modules.Import_Declaration)
   is
      use type Leander.Environment.Reference;

      Imported : constant Leander.Environment.Reference :=
                   Context.Load_Module_By_Name (Name, From_Dir);

      Mode : constant Leander.Environment.Import_Visibility :=
               (if Import.Is_Qualified then Leander.Environment.No_Names
                elsif not Import.Has_Name_List
                then Leander.Environment.All_Names
                elsif Import.Is_Hiding
                then Leander.Environment.Except_Names
                else Leander.Environment.Only_Names);

      function Visible_Names return Leander.Names.Name_Array;
      function Sub_Names_Of (Name : String)
        return Leander.Names.Name_Array;
      function Wildcard_Constructors (From : Positive)
        return Leander.Names.Name_Array;

      -------------------
      -- Sub_Names_Of --
      -------------------

      function Sub_Names_Of (Name : String)
        return Leander.Names.Name_Array
      is
         Id : constant Leander.Core.Conid := Leander.Core.To_Conid (Name);
      begin
         --  What "(..)" stands for depends on what was named: a data
         --  type's constructors, or a class's methods.
         if Imported.Exists
           (Leander.Names.To_Leander_Name (Name),
            Leander.Environment.Type_Constructor)
         then
            declare
               DT : constant Leander.Data_Types.Reference :=
                      Imported.Data_Type (Id);
            begin
               return R : Leander.Names.Name_Array
                            (1 .. DT.Constructor_Count)
               do
                  for J in R'Range loop
                     R (J) :=
                       Leander.Names.To_Leander_Name
                         (Leander.Core.To_String (DT.Constructor_Name (J)));
                  end loop;
               end return;
            end;
         elsif Imported.Exists
           (Leander.Names.To_Leander_Name (Name),
            Leander.Environment.Class_Binding)
         then
            declare
               Methods : constant Leander.Core.Varid_Array :=
                           Imported.Get_Class (Id).Methods;
            begin
               return R : Leander.Names.Name_Array (Methods'Range) do
                  for J in R'Range loop
                     R (J) :=
                       Leander.Names.To_Leander_Name
                         (Leander.Core.To_String (Methods (J)));
                  end loop;
               end return;
            end;
         else
            return [];
         end if;
      end Sub_Names_Of;

      ---------------------------
      -- Wildcard_Constructors --
      ---------------------------

      function Wildcard_Constructors (From : Positive)
        return Leander.Names.Name_Array
      is
         use type Leander.Names.Name_Array;
      begin
         if From > Import.Wildcard_Count then
            return [];
         else
            return Sub_Names_Of (Import.Wildcard (From))
              & Wildcard_Constructors (From + 1);
         end if;
      end Wildcard_Constructors;

      -------------------
      -- Visible_Names --
      -------------------

      function Visible_Names return Leander.Names.Name_Array is
         use type Leander.Names.Name_Array;
         Written : Leander.Names.Name_Array (1 .. Import.Name_Count);
      begin
         for I in Written'Range loop
            Written (I) := Leander.Names.To_Leander_Name (Import.Name (I));
         end loop;

         --  "T(..)" asks for whatever constructors T turns out to have,
         --  which only the module being imported can say -- so it is
         --  expanded here rather than where it was written.
         return Written & Wildcard_Constructors (1);
      end Visible_Names;

   begin
      if Imported = null then
         if Leander.Parser.Module_Is_Loading (Name) then
            Error ("import cycle: " & Name
                   & " is already being loaded");
         else
            Error ("could not find module " & Name);
         end if;
      elsif Imported = Env then
         Error ("module " & Name & " cannot import itself");
      else
         Env.Import (Imported, Mode, Visible_Names);
         Context.Scope.Add_Module (Alias, Imported);

      end if;
   end Do_Import;

   ------------------
   -- Parse_Module --
   ------------------

   function Parse_Module
     (Context  : in out Parse_Context'Class;
      Name     : String;
      From_Dir : String)
      return Leander.Environment.Reference
   is
      Exports    : Leander.Syntax.Modules.Export_List;
      Export_Loc : Leander.Source.Source_Location :=
                     Leander.Source.No_Location;
      Env        : constant Leander.Environment.Reference :=
                  (if Name = "Prelude"
                   then Leander.Environment.Prelude.Create
                   else Leander.Environment.New_Environment (Name));
   begin
      Context.New_Environment (Env);

      --  A scope table per module, so that one module's aliases are not
      --  visible from another. Load_Module_By_Name saves and restores it
      --  alongside the environment, which is what makes a module loaded
      --  partway through another module's parse safe.
      Context.Set_Scope (Leander.Scopes.New_Scope);

      if Tok = Tok_Left_Paren then
         Export_Loc := Current_Source_Location;
         Parse_Export_List (Exports);
      end if;

      Expect (Tok_Where,
              [Tok_Identifier, Tok_Data, Tok_Type, Tok_Newtype,
               Tok_Class, Tok_Instance]);

      --  The Prelude is imported by every module but its own, ahead of
      --  any explicit import, so that its classes and constructors are in
      --  place before declarations are parsed. Writing it as an ordinary
      --  import rather than a special case is what will later let
      --  "import Prelude ()" and "import qualified Prelude" mean anything.
      if Name /= "Prelude" then
         declare
            Implicit : Leander.Syntax.Modules.Import_Declaration;
         begin
            Implicit.Start_Import ("Prelude", Qualified => False);
            Do_Import (Context, Env, "Prelude", Alias => "Prelude",
                       From_Dir => "", Import => Implicit);
         end;
      end if;

      while Tok = Tok_Import loop
         Parse_Import (Context, Env, From_Dir);
      end loop;

      begin
         Declarations.Parse_Declarations (Context);

         Expect (Tok_End_Of_File, Tok_End_Of_File);

      exception
         when Parse_Error =>
            null;
      end;

      --  Applying Exports belongs here, after the declarations exist:
      --  "T(..)" cannot name its constructors, and "exports a name this
      --  module never declared" cannot be detected, any earlier.
      if Exports.Is_Present then
         Apply_Exports (Env, Exports, Export_Loc);
      end if;

      return Env;
   end Parse_Module;

   -----------------------
   -- Parse_Export_List --
   -----------------------

   procedure Parse_Export_List
     (Exports : in out Leander.Syntax.Modules.Export_List)
   is
      procedure Parse_Entry;

      -----------------
      -- Parse_Entry --
      -----------------

      procedure Parse_Entry is
      begin
         if Tok = Tok_Module then
            --  A module re-export's meaning depends on other modules'
            --  export sets, and it interacts with the transitive type and
            --  class copy in exactly the hardest way. Parse it so that the
            --  rest of the list still scans, then say no.
            Scan;
            if At_Name then
               Error ("module re-exports are not supported: module "
                      & Scan_Dotted_Name);
            else
               Error ("expected a module name");
            end if;
            return;
         end if;

         if not At_Name then
            Error ("expected an exported name");
            return;
         end if;

         declare
            Name : constant String := Scan_Identifier;
         begin
            if Tok /= Tok_Left_Paren then
               Exports.Add_Export (Name);
               return;
            end if;

            --  T(..) or T(Con, Con)
            Scan;

            if Tok = Tok_Dot_Dot then
               Scan;
               Exports.Add_Type_Export (Name, With_Constructors => True);
            else
               Exports.Add_Type_Export (Name, With_Constructors => False);
               while At_Name loop
                  Exports.Add_Constructor_Export (Name, Scan_Identifier);
                  exit when Tok /= Tok_Comma;
                  Scan;
               end loop;
            end if;

            if Tok = Tok_Right_Paren then
               Scan;
            else
               Error ("missing ')'");
            end if;
         end;
      end Parse_Entry;

   begin
      Exports.Start_Export_List;
      Scan;

      if Tok /= Tok_Right_Paren then
         loop
            Parse_Entry;
            exit when Tok /= Tok_Comma;
            Scan;
         end loop;
      end if;

      if Tok = Tok_Right_Paren then
         Scan;
      else
         Error ("missing ')' after the export list");
      end if;
   end Parse_Export_List;

   ------------------
   -- Parse_Import --
   ------------------

   procedure Parse_Import
     (Context  : in out Parse_Context'Class;
      Env      : Leander.Environment.Reference;
      From_Dir : String)
   is
      Import    : Leander.Syntax.Modules.Import_Declaration;
      Qualified : Boolean := False;
   begin
      Scan;

      if At_Soft_Keyword ("qualified", Tok_Identifier) then
         Qualified := True;
         Scan;
      end if;

      if not At_Name then
         Error ("expected a module name");
         while Tok_Indent > 1 loop
            Scan;
         end loop;
         return;
      end if;

      Import.Start_Import (Scan_Dotted_Name, Qualified);

      if At_Soft_Keyword ("as", Tok_Identifier) then
         Scan;
         Import.Set_Alias (Scan_Dotted_Name);
      end if;

      if At_Soft_Keyword ("hiding", Tok_Left_Paren) then
         Scan;
         Import.Start_Name_List (Hiding => True);
         Scan;
      elsif Tok = Tok_Left_Paren then
         Import.Start_Name_List (Hiding => False);
         Scan;
      end if;

      if Import.Has_Name_List then
         if Tok /= Tok_Right_Paren then
            loop
               if not At_Name then
                  Error ("expected an imported name");
                  exit;
               end if;

               Import.Add_Name (Scan_Identifier);

               --  T(..) and T(Con, Con) select constructors alongside the
               --  type; the names themselves are what the scope table will
               --  want, so flatten them into the same list.
               if Tok = Tok_Left_Paren then
                  Scan;
                  if Tok = Tok_Dot_Dot then
                     Import.Add_Wildcard_Name
                       (Import.Name (Import.Name_Count));
                     Scan;
                  else
                     while At_Name loop
                        Import.Add_Name (Scan_Identifier);
                        exit when Tok /= Tok_Comma;
                        Scan;
                     end loop;
                  end if;

                  if Tok = Tok_Right_Paren then
                     Scan;
                  else
                     Error ("missing ')'");
                  end if;
               end if;

               exit when Tok /= Tok_Comma;
               Scan;
            end loop;
         end if;

         if Tok = Tok_Right_Paren then
            Scan;
         else
            Error ("missing ')' after the import list");
         end if;
      end if;

      Do_Import (Context, Env, Import.Module_Name, Import.Alias,
                 From_Dir, Import);
   end Parse_Import;

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
