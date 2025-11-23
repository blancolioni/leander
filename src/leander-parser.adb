with Ada.Characters.Handling;
with Ada.Directories;

with Leander.Parser.Lexical;           use Leander.Parser.Lexical;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;

with Leander.Parser.Expressions;
with Leander.Parser.Modules;

with WL.String_Maps;

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

   Loaded_Module_Map : Loaded_Module_Maps.Map;

   ---------------
   -- Add_Class --
   ---------------

   procedure Add_Class
     (This : in out Parse_Context'Class;
      Name : String)
   is
   begin
      This.Known_Classes.Append (Name);
   end Add_Class;

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

   -----------------
   -- Known_Class --
   -----------------

   function Known_Class
     (This : Parse_Context'Class;
      Name : String)
      return Boolean
   is
   begin
      return This.Known_Classes.Contains (Name);
   end Known_Class;

   -----------------
   -- Load_Module --
   -----------------

   function Load_Module
     (Context : in out Parse_Context'Class;
      Path    : String)
      return Leander.Environment.Reference
   is
      Name : constant String :=
               Ada.Directories.Base_Name (Path);
   begin
      if Loaded_Module_Map.Contains (Name) then
         return Loaded_Module_Map (Name);
      else
         Open (Path);
         return Env : constant Leander.Environment.Reference :=
           Leander.Parser.Modules.Parse_Module (Context, Name)
         do
            Loaded_Module_Map.Insert (Name, Env);
            Close;

            if Name /= "Prelude" then
               Env.Import
                 (Context.Load_Module ("./shared/leander/modules/Prelude.hs"));
            end if;

            Env.Elaborate;
            Leander.Syntax.Prune;
         end return;
      end if;
   end Load_Module;

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
