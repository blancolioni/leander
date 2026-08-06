with Ada.Strings.Unbounded;

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

   function Parse_Expression
     (Context : Parse_Context'Class;
      Expr    : String)
      return Leander.Syntax.Expressions.Reference;

   function Load_Module
     (Context : in out Parse_Context'Class;
      Path    : String)
      return Leander.Environment.Reference;

   procedure Register_Loaded_Module
     (Context : in out Parse_Context'Class;
      Name    : String;
      Env     : Leander.Environment.Reference);
   --  Record Env as Name's already-loaded module, so a later Load_Module
   --  (Context, Path) for a source file whose base name is Name returns Env
   --  directly rather than parsing -- for a module reconstructed entirely
   --  from a complete .skix image (see Leander.Handles.Create), so that
   --  module's source is never opened at all, not even by a second,
   --  unrelated Load_Module call that would otherwise re-parse it.

   procedure Add_Fixity
     (Operator      : String;
      Associativity : Natural;
      Priority      : Natural);
   --  Restore a single decoded operator fixity declaration (Associativity:
   --  0 = infixl, 1 = infixr, 2 = infix; Priority: 0 .. 9) without parsing
   --  an "infixl"/"infixr"/"infix" declaration for it.

   type Fixity_Entry is
      record
         Operator      : Ada.Strings.Unbounded.Unbounded_String;
         Associativity : Natural;
         Priority      : Natural;
      end record;

   type Fixity_Entry_Array is array (Positive range <>) of Fixity_Entry;

   function All_Fixities return Fixity_Entry_Array;
   --  Every operator fixity declaration registered so far, for Dump_Module
   --  to encode into a module's .skix image.

   function Current_Source_Location return Leander.Source.Source_Location;

private

   function Is_Alphanumeric_Identifier (Name : String) return Boolean;
   function Is_Symbolic_Identifier (Name : String) return Boolean;
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

   type Parse_Context is tagged
      record
         Env : Leander.Environment.Reference;
      end record;

   function Environment
     (This : Parse_Context'Class)
      return Leander.Environment.Reference
   is (This.Env);

end Leander.Parser;
