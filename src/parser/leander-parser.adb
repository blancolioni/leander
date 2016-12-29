with Leander.Parser.Lexical;           use Leander.Parser.Lexical;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;

with Leander.Parser.Expressions;
with Leander.Parser.Modules;

package body Leander.Parser is

   Next_Variable_Index : Positive := 1;

   --------------------
   -- At_Constructor --
   --------------------

   function At_Constructor return Boolean is
      Text : constant String := Tok_Text;
   begin
      if Tok /= Tok_Identifier then
         return False;
      end if;
      return Text (Text'First) in 'A' .. 'Z'
        or else Text (Text'First) = ':';
   end At_Constructor;

   -----------------
   -- At_Variable --
   -----------------

   function At_Variable return Boolean is
   begin
      return Tok = Tok_Identifier
        and then not At_Constructor;
   end At_Variable;

   ------------------------------
   -- Current_Source_Reference --
   ------------------------------

   function Current_Source_Reference return Leander.Source.Source_Reference is
   begin
      return Leander.Source.Create_Reference
        (Tok_File_Name, Tok_Line, Tok_Column);
   end Current_Source_Reference;

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
      return Leander.Syntax.Syntax_Tree
   is
   begin
      Open_String (Expr);
      declare
         Result : constant Leander.Syntax.Syntax_Tree :=
                    Leander.Parser.Expressions.Parse_Expression;
      begin
         Close;
         return Result;
      end;
   end Parse_Expression;

end Leander.Parser;
