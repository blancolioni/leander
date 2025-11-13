with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Leander.Environment;
with Leander.Syntax.Expressions;

with Leander.Source;

package Leander.Parser is

   Parse_Error : exception;

   type Parse_Context is tagged private;

   procedure New_Environment
     (This : in out Parse_Context'Class;
      Env  : Leander.Environment.Reference);

   function Environment
     (This : Parse_Context'Class)
      return Leander.Environment.Reference;

   function Known_Class
     (This : Parse_Context'Class;
      Name : String)
      return Boolean;

   procedure Add_Class
     (This : in out Parse_Context'Class;
      Name : String);

   function Parse_Expression
     (Context : Parse_Context'Class;
      Expr    : String)
      return Leander.Syntax.Expressions.Reference;

   function Load_Module
     (Context : in out Parse_Context'Class;
      Path    : String)
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

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Environment_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Leander.Environment.Reference,
        Leander.Environment."=");

   type Parse_Context is tagged
      record
         Env           : Leander.Environment.Reference;
         Known_Classes : String_Lists.List;
      end record;

   function Environment
     (This : Parse_Context'Class)
      return Leander.Environment.Reference
   is (This.Env);

end Leander.Parser;
