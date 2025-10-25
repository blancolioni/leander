with Leander.Environment;
with Leander.Syntax.Expressions;

with Leander.Source;

package Leander.Parser is

   Parse_Error : exception;

   function Parse_Expression
     (Expr : String)
      return Leander.Syntax.Expressions.Reference;

   function Load_Module
     (Path : String)
      return Leander.Environment.Reference;

   function Current_Source_Location return Leander.Source.Source_Location;

private

   function Is_Constructor (Name : String) return Boolean;

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

end Leander.Parser;
