with Ada.Characters.Handling;

with Leander.Parser.Lexical;           use Leander.Parser.Lexical;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;

with Leander.Parser.Expressions;
with Leander.Parser.Modules;

package body Leander.Parser is

   Next_Variable_Index : Positive := 1;

   function Is_Alphanumeric_Identifier (Name : String) return Boolean
   is (Ada.Characters.Handling.Is_Letter (Name (Name'First))
       or else Name (Name'First) in '_' | '#');

   function Is_Symbolic_Identifier (Name : String) return Boolean
   is (not Is_Alphanumeric_Identifier (Name));

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

   ------------------------------
   -- Current_Source_Reference --
   ------------------------------

   function Current_Source_Reference return Leander.Source.Source_Reference is
   begin
      return Leander.Source.Create_Reference
        (Tok_File_Name, Tok_Line, Tok_Column);
   end Current_Source_Reference;

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

   -------------------
   -- Import_Module --
   -------------------

   procedure Import_Module
     (Name : String;
      Path : String;
      Env  : in out Leander.Environments.Environment)
   is
      Module : constant Leander.Environments.Environment :=
                 Leander.Parser.Modules.Load_Module
                   (Name, Path);
   begin
      Env.Import_Names (Module);
   end Import_Module;

   ------------------
   -- New_Variable --
   ------------------

   function New_Variable return String is
      Img : String := Positive'Image (Next_Variable_Index);
   begin
      Next_Variable_Index := Next_Variable_Index + 1;
      Img (Img'First) := '-';
      return "x" & Img;
   end New_Variable;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     (Expr : String)
      return Leander.Core.Trees.Tree_Type
   is
   begin
      Open_String (Expr);
      declare
         Result : constant Leander.Core.Trees.Tree_Type :=
                    Leander.Parser.Expressions.Parse_Expression;
      begin
         Close;
         return Result;
      end;
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
