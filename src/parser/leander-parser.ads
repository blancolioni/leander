with Leander.Environments;
with Leander.Source;
with Leander.Syntax;

package Leander.Parser is

   function Parse_Expression
     (Expr : String)
      return Leander.Syntax.Syntax_Tree;

   procedure Import_Module
     (Name : String;
      Path : String;
      Env  : in out Leander.Environments.Environment);

private

   function Current_Source_Reference return Leander.Source.Source_Reference;

   function At_Constructor return Boolean;
   function At_Variable return Boolean;

   function At_Name return Boolean;
   function At_Operator return Boolean;

   function At_Constructor_Name return Boolean
   is (At_Name and then At_Constructor);

   function At_Variable_Name return Boolean
   is (At_Name and then At_Variable);

   function At_Constructor_Op return Boolean
   is (At_Operator and then At_Constructor);

   function At_Variable_Op return Boolean
   is (At_Operator and then At_Variable);

   function At_Identifier return Boolean
   is (At_Name or else At_Operator);

   function Get_Identifier return String
     with Pre => At_Name or else At_Operator;

   function Scan_Identifier return String
     with Pre => At_Identifier;

   function New_Variable return String;

end Leander.Parser;
