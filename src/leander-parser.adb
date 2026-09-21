with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Strings.Fixed;

with GCS.Constraints;

with Leander.Parser.Lexical;           use Leander.Parser.Lexical;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;

with Leander.Parser.Expressions;
with Leander.Parser.Modules;

with Leander.Resources;

with WL.String_Maps;
with WL.String_Sets;

package body Leander.Parser is

   function Is_Alphanumeric_Identifier (Name : String) return Boolean
   is (Ada.Characters.Handling.Is_Letter (Name (Name'First))
       or else Name (Name'First) in '_' | '#');

   function Is_Symbolic_Identifier (Name : String) return Boolean
   is (not Is_Alphanumeric_Identifier (Name));

   function Is_Constructor (Name : String) return Boolean
   is (Name (Name'First) in 'A' .. 'Z'
       or else Name (Name'First) = ':');

   package Loaded_Module_Maps is
     new WL.String_Maps (Leander.Environment.Reference,
                         Leander.Environment."=");

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   Loaded_Module_Map : Loaded_Module_Maps.Map;
   --  Keyed by dotted module name, as read from a module's own header.
   --  This is the authoritative "already loaded" answer: a module is
   --  loaded once however many files or import declarations name it.

   Loaded_Path_Map   : Loaded_Module_Maps.Map;
   --  Keyed by full source path, so that loading the same file twice does
   --  not have to reopen it just to read its header. A miss here is only
   --  ever a cost, never an error: the name map above still catches it.

   Loading_Modules   : WL.String_Sets.Set;
   --  Modules whose parse is in progress, so that a cycle is a diagnostic
   --  rather than a recursion down to the lexer's open-file limit.

   Include_Path_List : String_Vectors.Vector;

   function Module_Source_Name (Name : String) return String;
   --  "Data.List" -> "Data/List.hs"

   function Join (Directory, Relative : String) return String
   is (if Directory = "" then Relative
       elsif Directory (Directory'Last) in '/' | '\'
       then Directory & Relative
       else Directory & '/' & Relative);


   ------------------
   -- Add_Fixity --
   ------------------

   procedure Add_Fixity
     (Operator      : String;
      Associativity : Natural;
      Priority      : Natural)
   is
   begin
      Leander.Parser.Expressions.Set_Fixity
        (Operator,
         Leander.Parser.Expressions.Associativity_Type'Val (Associativity),
         Leander.Parser.Expressions.Priority_Range (Priority));
   end Add_Fixity;

   ------------------
   -- All_Fixities --
   ------------------

   function All_Fixities return Fixity_Entry_Array is
      Info : constant Leander.Parser.Expressions.Fixity_Info_Array :=
               Leander.Parser.Expressions.All_Fixities;
   begin
      return
        [for E of Info =>
           Fixity_Entry'
             (Operator      => E.Operator,
              Associativity =>
                Leander.Parser.Expressions.Associativity_Type'Pos
                  (E.Associativity),
              Priority      => Natural (E.Priority))];
   end All_Fixities;

   --------------------
   -- At_Constructor --
   --------------------

   function At_Constructor return Boolean is
   begin
      if At_Identifier then
         declare
            Name : constant String := Get_Identifier;
         begin
            return Name (Name'First) in 'A' .. 'Z'
              or else Name (Name'First) = ':';
         end;
      else
         return False;
      end if;
   end At_Constructor;

   -------------
   -- At_Name --
   -------------

   function At_Name return Boolean is
   begin
      if Tok = Tok_Identifier then
         return Is_Alphanumeric_Identifier (Tok_Text);
      elsif Tok = Tok_Left_Paren
        and then Next_Tok (1) = Tok_Identifier
        and then Next_Tok (2) = Tok_Right_Paren
      then
         return Is_Symbolic_Identifier (Tok_Text (1));
      else
         return False;
      end if;
   end At_Name;

   -----------------
   -- At_Operator --
   -----------------

   function At_Operator return Boolean is
   begin
      if Tok = Tok_Identifier then
         return Is_Symbolic_Identifier (Tok_Text);
      elsif Tok = Tok_Back_Tick
        and then Next_Tok (1) = Tok_Identifier
        and then Next_Tok (2) = Tok_Back_Tick
      then
         return Is_Alphanumeric_Identifier (Tok_Text (1));
      else
         return False;
      end if;
   end At_Operator;

   -----------------
   -- At_Variable --
   -----------------

   function At_Variable return Boolean is
   begin
      return At_Identifier and then not At_Constructor;
   end At_Variable;

   -----------------------------
   -- Current_Source_Location --
   -----------------------------

   function Current_Source_Location return Leander.Source.Source_Location is
   begin
      return Leander.Source.Create_Location
        (Tok_File_Name, Tok_Line, Tok_Column);
   end Current_Source_Location;

   --------------------
   -- Get_Identifier --
   --------------------

   function Get_Identifier return String is
   begin
      if Tok = Tok_Identifier then
         return Tok_Text;
      else
         return Tok_Text (1);
      end if;
   end Get_Identifier;

   ----------------------
   -- Add_Include_Path --
   ----------------------

   procedure Add_Include_Path (Dir : String) is
   begin
      Include_Path_List.Append (Dir);
   end Add_Include_Path;

   -------------------------
   -- Last_Name_Component --
   -------------------------

   function Last_Name_Component (Name : String) return String is
      Dot : constant Natural :=
              Ada.Strings.Fixed.Index (Name, ".", Ada.Strings.Backward);
   begin
      return (if Dot = 0 then Name else Name (Dot + 1 .. Name'Last));
   end Last_Name_Component;

   -----------------
   -- Load_Module --
   -----------------

   function Load_Module
     (Context : in out Parse_Context'Class;
      Path    : String)
      return Leander.Environment.Reference
   is
      --  GCS prepends the directory of the first file it ever opened to
      --  any later relative name (gcs-file_manager.adb), which for us is
      --  wherever Prelude came from. Resolving to a full name first keeps
      --  a module's own path meaning what it says.
      Full : constant String := Ada.Directories.Full_Name (Path);
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Env  : Leander.Environment.Reference;
   begin
      if Loaded_Path_Map.Contains (Full) then
         return Loaded_Path_Map (Full);
      end if;

      Open (Full);

      begin
         Name :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Leander.Parser.Modules.Scan_Module_Header);
      exception
         when others =>
            Close;
            raise;
      end;

      declare
         use Ada.Strings.Unbounded;
         Module : constant String := To_String (Name);
         Base   : constant String := Ada.Directories.Base_Name (Path);
      begin
         if Module = "" then
            Close;
            return null;
         end if;

         --  The header is what names the module, but a file claiming a
         --  name its own file name contradicts would poison the cache for
         --  anything later importing either name, so say so.
         if Last_Name_Component (Module) /= Base then
            Error ("module " & Module & " should be in a file named "
                   & Last_Name_Component (Module) & ".hs");
         end if;

         if Loaded_Module_Map.Contains (Module) then
            Close;
            Env := Loaded_Module_Map (Module);
            Loaded_Path_Map.Insert (Full, Env);
            return Env;
         end if;

         if Loading_Modules.Contains (Module) then
            Error ("module " & Module & " is part of an import cycle");
            Close;
            return null;
         end if;

         Loading_Modules.Include (Module);

         --  Parse_Module imports the Prelude and then whatever the module's
         --  own import declarations name, all with this file's lexer frame
         --  already open. That nesting is safe: the lexer keeps its token
         --  state per open file.
         begin
            Env := Leander.Parser.Modules.Parse_Module
              (Context, Module,
               From_Dir => Ada.Directories.Containing_Directory (Full));
         exception
            when others =>
               --  Parse_Module absorbs Parse_Error itself, but anything it
               --  lets through would leak this lexer frame. Loading one
               --  module from inside another compounds that, so close the
               --  frame we opened before propagating.
               Loading_Modules.Delete (Module);
               Close;
               raise;
         end;

         Loading_Modules.Delete (Module);
         Loaded_Module_Map.Insert (Module, Env);
         Loaded_Path_Map.Insert (Full, Env);
         Close;
         Env.Elaborate;
         return Env;
      end;
   end Load_Module;

   -------------------------
   -- Load_Module_By_Name --
   -------------------------

   function Load_Module_By_Name
     (Context  : in out Parse_Context'Class;
      Name     : String;
      From_Dir : String)
      return Leander.Environment.Reference
   is
      --  Parse_Module points the context at whatever module it is
      --  building, so a load started from inside a parse would otherwise
      --  leave the importer parsing against the imported module's
      --  environment.
      Saved       : constant Leander.Environment.Reference := Context.Env;
      Saved_Scope : constant Leander.Scopes.Reference := Context.Scope;
   begin
      if Loaded_Module_Map.Contains (Name) then
         return Loaded_Module_Map (Name);
      end if;

      if Loading_Modules.Contains (Name) then
         return null;
      end if;

      declare
         Path : constant String := Resolve_Module_Path (Name, From_Dir);
      begin
         if Path = "" then
            return null;
         end if;

         return Env : constant Leander.Environment.Reference :=
           Context.Load_Module (Path)
         do
            Context.Env := Saved;
            Context.Scope := Saved_Scope;
         end return;
      exception
         when others =>
            Context.Env := Saved;
            Context.Scope := Saved_Scope;
            raise;
      end;
   end Load_Module_By_Name;

   -----------------------
   -- Module_Is_Loading --
   -----------------------

   function Module_Is_Loading (Name : String) return Boolean
   is (Loading_Modules.Contains (Name));

   ------------------------
   -- Module_Source_Name --
   ------------------------

   function Module_Source_Name (Name : String) return String is
      Result : String := Name;
   begin
      for Ch of Result loop
         if Ch = '.' then
            Ch := '/';
         end if;
      end loop;
      return Result & ".hs";
   end Module_Source_Name;

   ----------------------------
   -- Register_Loaded_Module --
   ----------------------------

   procedure Register_Loaded_Module
     (Context : in out Parse_Context'Class;
      Name    : String;
      Env     : Leander.Environment.Reference;
      Path    : String := "")
   is
      pragma Unreferenced (Context);
   begin
      if not Loaded_Module_Map.Contains (Name) then
         Loaded_Module_Map.Insert (Name, Env);
      end if;

      if Path /= "" then
         declare
            Full : constant String := Ada.Directories.Full_Name (Path);
         begin
            if not Loaded_Path_Map.Contains (Full) then
               Loaded_Path_Map.Insert (Full, Env);
            end if;
         end;
      end if;
   end Register_Loaded_Module;

   -------------------------
   -- Resolve_Module_Path --
   -------------------------

   function Resolve_Module_Path
     (Name     : String;
      From_Dir : String)
      return String
   is
      Relative : constant String := Module_Source_Name (Name);

      function Try (Directory : String) return String;

      ---------
      -- Try --
      ---------

      function Try (Directory : String) return String is
         Path : constant String := Join (Directory, Relative);
      begin
         if Ada.Directories.Exists (Path)
           and then Ada.Directories."="
                      (Ada.Directories.Kind (Path),
                       Ada.Directories.Ordinary_File)
         then
            return Ada.Directories.Full_Name (Path);
         else
            return "";
         end if;
      exception
         when others =>
            --  A directory that does not exist, or a name this platform
            --  will not even form, is just a miss.
            return "";
      end Try;

   begin
      if From_Dir /= "" then
         declare
            Found : constant String := Try (From_Dir);
         begin
            if Found /= "" then
               return Found;
            end if;
         end;
      end if;

      for Directory of Include_Path_List loop
         declare
            Found : constant String := Try (Directory);
         begin
            if Found /= "" then
               return Found;
            end if;
         end;
      end loop;

      return Try (Leander.Resources.Resource_Path & "modules");
   end Resolve_Module_Path;

   ---------------------
   -- New_Environment --
   ---------------------

   procedure New_Environment
     (This : in out Parse_Context'Class;
      Env  : Leander.Environment.Reference)
   is
   begin
      This.Env := Env;
   end New_Environment;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     (Context : Parse_Context'Class;
      Expr    : String)
      return Leander.Syntax.Expressions.Reference
   is
   begin
      Open_String (Expr);
      return Result : constant Leander.Syntax.Expressions.Reference :=
        Leander.Parser.Expressions.Parse_Expression (Context)
      do
         Close;
      end return;
   end Parse_Expression;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (This    : Parse_Context'Class;
      Written : String;
      Space   : Leander.Scopes.Name_Space)
      return String
   is
      use Ada.Strings.Unbounded;
      use type Leander.Scopes.Reference;
      Name    : Unbounded_String;
      Message : Unbounded_String;
   begin
      if This.Scope = null then
         return Written;
      end if;

      This.Scope.Resolve (Written, Space, Name, Message);

      if Message /= Null_Unbounded_String then
         Error (To_String (Message));
      end if;

      return To_String (Name);
   end Resolve;

   -------------------------
   -- Scan_Qualified_Name --
   -------------------------

   function Scan_Qualified_Name return String is
      use Ada.Strings.Unbounded;

      Result    : Unbounded_String;
      Line      : GCS.Constraints.Line_Number;
      Next_Col  : GCS.Constraints.Column_Count;
      Qualifier : Boolean;

      procedure Take_Component;
      function Adjacent return Boolean;

      --------------
      -- Adjacent --
      --------------

      function Adjacent return Boolean
      is (Tok = Tok_Identifier
          and then Tok_Line = Line
          and then Tok_Column = Next_Col);
      --  Tok_Info.Finish is never assigned (gcs-lexer.adb), so the end of
      --  a token is its column plus its length. Gating on Tok_Identifier
      --  is what keeps "[A..B]" safe: ".." lexes as Tok_Dot_Dot.

      --------------------
      -- Take_Component --
      --------------------

      procedure Take_Component is
      begin
         Append (Result, Tok_Text);
         Line := Tok_Line;
         Next_Col := Tok_Column + Tok_Text'Length;
         Scan;
      end Take_Component;

   begin
      Qualifier := Is_Constructor (Tok_Text);
      Take_Component;

      while Qualifier and then Adjacent loop
         declare
            Run : constant String := Tok_Text;
         begin
            exit when Run (Run'First) /= '.';

            if Run'Length > 1 then
               --  The lexer groups a run of symbolic characters together,
               --  so "S.+" arrives as "S" then ".+" -- one token holding
               --  both the dot and the operator it qualifies. A run
               --  starting ".." is Tok_Dot_Dot's shape and never a name.
               exit when Run (Run'First + 1) = '.';
               Append (Result, Run);
               Scan;
               exit;
            end if;

            Line := Tok_Line;
            Next_Col := Tok_Column + 1;
            Scan;

            if not Adjacent then
               --  The dot is consumed and the lexer cannot push a token
               --  back, so there is nothing to do but report it.
               Error ("expected a name after '.'");
               exit;
            end if;

            Append (Result, ".");
            Qualifier := Is_Constructor (Tok_Text);
            Take_Component;
         end;
      end loop;

      return To_String (Result);
   end Scan_Qualified_Name;

   ---------------
   -- Set_Scope --
   ---------------

   procedure Set_Scope
     (This  : in out Parse_Context'Class;
      Scope : Leander.Scopes.Reference)
   is
   begin
      This.Scope := Scope;
   end Set_Scope;

   -----------------------
   -- Tok_Is_Identifier --
   -----------------------

   function Tok_Is_Identifier return Boolean
   is (Tok = Tok_Identifier);

   ----------------------
   -- Scan_Dotted_Name --
   ----------------------

   function Scan_Dotted_Name return String is
      use Ada.Strings.Unbounded;
      Result   : Unbounded_String;
      Line     : GCS.Constraints.Line_Number;
      Next_Col : GCS.Constraints.Column_Count;
   begin
      loop
         Line := Tok_Line;
         Next_Col := Tok_Column + Tok_Text'Length;
         Append (Result, Tok_Text);
         Scan;

         --  The lexer hands back "." as an ordinary symbolic identifier,
         --  so adjacency is the only thing that distinguishes a qualified
         --  name from a composition. Tok_Info.Finish is never assigned
         --  (gcs-lexer.adb), hence the column arithmetic.
         exit when Tok /= Tok_Identifier
           or else Tok_Text /= "."
           or else Tok_Line /= Line
           or else Tok_Column /= Next_Col;

         Next_Col := Next_Col + 1;
         Scan;

         if Tok /= Tok_Identifier
           or else Tok_Line /= Line
           or else Tok_Column /= Next_Col
         then
            --  The dot is already consumed and the lexer cannot push a
            --  token back, so there is nothing to do but report it.
            Error ("expected a module name component after '.'");
            exit;
         end if;

         Append (Result, ".");
      end loop;

      return To_String (Result);
   end Scan_Dotted_Name;

   ---------------------
   -- Scan_Identifier --
   ---------------------

   function Scan_Identifier return String is
      Name : constant String :=
        (if Tok = Tok_Identifier
         then Tok_Text
         else Tok_Text (1));
   begin
      if Tok = Tok_Identifier then
         Scan;
      else
         Scan;
         Scan;
         Scan;
      end if;
      return Name;
   end Scan_Identifier;

end Leander.Parser;
