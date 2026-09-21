with Ada.Strings.Unbounded;

with Leander.Environment;
with Leander.Scopes;
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
   --  Load the module held in the source file Path. The module's own name
   --  comes from its header, not from the file name, so Path may sit
   --  anywhere; the file base name only has to match the header's last
   --  component.

   function Load_Module_By_Name
     (Context  : in out Parse_Context'Class;
      Name     : String;
      From_Dir : String)
      return Leander.Environment.Reference;
   --  Load the module called Name (dotted, e.g. "Data.List"), resolving it
   --  to a source file via Resolve_Module_Path below. Returns null if no
   --  such file exists, or if Name is already being loaded further up the
   --  chain (an import cycle). It does not report either case itself: the
   --  caller is what holds the source location worth pointing at, which is
   --  the import declaration that asked for the module. Safe to call from
   --  inside a parse -- the caller's current environment is restored
   --  before returning.

   function Module_Is_Loading (Name : String) return Boolean;
   --  True when module Name is partway through its own parse, i.e. a
   --  request for it now closes an import cycle. Load_Module_By_Name
   --  returns null for this as it does for a module that does not exist;
   --  this is how a caller tells the two apart in its diagnostic.

   procedure Add_Include_Path (Dir : String);
   --  Append Dir to the directories searched for an imported module's
   --  source, after the importing file's own directory and before the
   --  installed module directory. Pushed from Leander.Driver, so that the
   --  parser needs no dependency on Leander.Command_Line.

   function Resolve_Module_Path
     (Name     : String;
      From_Dir : String)
      return String;
   --  The source file holding module Name, or "" if there is none. Dots
   --  become directory separators, so "Data.List" is "Data/List.hs" under
   --  each searched directory in turn: From_Dir (when not ""), then each
   --  Add_Include_Path directory in order, then the installed modules
   --  directory. Exposed for testing the search order without loading.

   procedure Register_Loaded_Module
     (Context : in out Parse_Context'Class;
      Name    : String;
      Env     : Leander.Environment.Reference;
      Path    : String := "");
   --  Record Env as module Name's already-loaded environment, so a later
   --  Load_Module_By_Name (Context, Name, ...) returns Env directly rather
   --  than parsing -- for a module reconstructed entirely from a complete
   --  .skix image (see Leander.Handles.Create).
   --
   --  Path, when given, is the source file Env stands in for, and is
   --  registered too. That part matters: Load_Module reads a file's header
   --  to learn its module name, so without the path a Load_Module call
   --  naming that same file would open the source after all, which is
   --  exactly what a full-coverage image is there to avoid.

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

   function Scan_Qualified_Name return String
     with Pre => Tok_Is_Identifier;
   --  Consume a name that may carry a module qualifier and return it as
   --  written, qualifier included: "Data.Map.insert", "S.Square", "S.+".
   --
   --  A dot joins the run only when it is written with no space either
   --  side and the component before it looks like a module name, which is
   --  Haskell's own rule and is what leaves "f . g" and "f.g" as
   --  composition. The run cannot be classified before it is consumed --
   --  "A.B.c" is five tokens against three of usable lookahead -- so
   --  callers must decide what they have from the result, not from the
   --  token they started on.

   function Scope (This : Parse_Context'Class) return Leander.Scopes.Reference;
   procedure Set_Scope
     (This  : in out Parse_Context'Class;
      Scope : Leander.Scopes.Reference);

   function Resolve
     (This    : Parse_Context'Class;
      Written : String;
      Space   : Leander.Scopes.Name_Space)
      return String;
   --  The name to use for Written, reporting at the current source
   --  location if it cannot be resolved. Falls back to Written on failure
   --  so that one bad name does not derail the rest of the parse.

   function Scan_Dotted_Name return String
     with Pre => At_Name;
   --  Consume a maximal run of adjacent identifiers joined by dots and
   --  return it dotted, e.g. "Data.List". A dot only joins the run when
   --  it is written with no space either side, which is Haskell's own
   --  rule and is what keeps "f . g" composition. Whether a run that
   --  could also be composition should have been reassembled at all is
   --  the caller's decision: in a module header or an import declaration
   --  there is nothing else it could be.

   function Last_Name_Component (Name : String) return String;
   --  The part of a dotted name after its final dot, i.e. the name itself
   --  with any module qualifier removed.

   function Tok_Is_Identifier return Boolean;
   --  Tok = Tok_Identifier, exposed so that Scan_Qualified_Name can state
   --  its precondition without the token type being visible here.

   type Parse_Context is tagged
      record
         Env   : Leander.Environment.Reference;
         Scope : Leander.Scopes.Reference;
      end record;

   function Scope (This : Parse_Context'Class) return Leander.Scopes.Reference
   is (This.Scope);

   function Environment
     (This : Parse_Context'Class)
      return Leander.Environment.Reference
   is (This.Env);

end Leander.Parser;
