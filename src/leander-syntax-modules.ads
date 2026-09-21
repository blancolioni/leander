private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;

package Leander.Syntax.Modules is

   --  Export lists and import declarations are declaration-shaped: they
   --  say what a module's boundary looks like, and have no To_Core. So
   --  they are held in builders in the style of Leander.Syntax.Classes
   --  rather than as arena-allocated Leander.Syntax nodes.

   ------------------
   -- Export lists --
   ------------------

   type Export_List is tagged private;

   procedure Start_Export_List (This : in out Export_List'Class);
   --  Record that the module wrote an export list at all. A module with
   --  no list exports everything it declares, which is not the same thing
   --  as a module with an empty one.

   procedure Add_Export
     (This : in out Export_List'Class;
      Name : String);
   --  A plain value, operator or type name: "foo", "(+)", "Maybe".

   procedure Add_Type_Export
     (This              : in out Export_List'Class;
      Name              : String;
      With_Constructors : Boolean);
   --  "Maybe(..)" when With_Constructors, otherwise as Add_Export. The
   --  constructors cannot be named until the data types exist, so this
   --  only records the request.

   procedure Add_Constructor_Export
     (This        : in out Export_List'Class;
      Type_Name   : String;
      Constructor : String);
   --  One constructor of "Maybe(Just, Nothing)".

   function Is_Present (This : Export_List'Class) return Boolean;
   function Count (This : Export_List'Class) return Natural;

   function Name
     (This  : Export_List'Class;
      Index : Positive)
      return String
     with Pre => Index <= This.Count;

   function Exports_Constructors
     (This  : Export_List'Class;
      Index : Positive)
      return Boolean
     with Pre => Index <= This.Count;
   --  True for an entry written "T(..)".

   function Constructor_Count
     (This  : Export_List'Class;
      Index : Positive)
      return Natural
     with Pre => Index <= This.Count;

   function Constructor
     (This        : Export_List'Class;
      Index       : Positive;
      Constructor : Positive)
      return String
     with Pre => Index <= This.Count
       and then Constructor <= This.Constructor_Count (Index);

   -------------------------
   -- Import declarations --
   -------------------------

   type Import_Declaration is tagged private;

   procedure Start_Import
     (This        : in out Import_Declaration'Class;
      Module_Name : String;
      Qualified   : Boolean);

   procedure Set_Alias
     (This  : in out Import_Declaration'Class;
      Alias : String);

   procedure Start_Name_List
     (This   : in out Import_Declaration'Class;
      Hiding : Boolean);

   procedure Add_Name
     (This : in out Import_Declaration'Class;
      Name : String);

   function Module_Name (This : Import_Declaration'Class) return String;

   function Is_Qualified (This : Import_Declaration'Class) return Boolean;

   function Alias (This : Import_Declaration'Class) return String;
   --  The name the module is written under: the "as" name when there is
   --  one, otherwise the module's own name.

   function Has_Name_List (This : Import_Declaration'Class) return Boolean;
   function Is_Hiding (This : Import_Declaration'Class) return Boolean;
   function Name_Count (This : Import_Declaration'Class) return Natural;

   function Name
     (This  : Import_Declaration'Class;
      Index : Positive)
      return String
     with Pre => Index <= This.Name_Count;

   function Is_Selective (This : Import_Declaration'Class) return Boolean;
   --  True when this import restricts or renames what it brings into
   --  scope, rather than bringing the whole module in unqualified. That
   --  is exactly the set of imports whose meaning depends on a scope
   --  table, so it is what tells the parser an import cannot yet be
   --  honoured in full.

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   use Ada.Strings.Unbounded;

   type Export_Entry is
      record
         Name              : Unbounded_String;
         With_Constructors : Boolean := False;
         Constructors      : String_Vectors.Vector;
      end record;

   package Export_Entry_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Export_Entry);

   type Export_List is tagged
      record
         Present : Boolean := False;
         Entries : Export_Entry_Vectors.Vector;
      end record;

   type Import_Declaration is tagged
      record
         Module_Name : Unbounded_String;
         Alias_Name  : Unbounded_String;
         Qualified   : Boolean := False;
         Name_List   : Boolean := False;
         Hiding      : Boolean := False;
         Names       : String_Vectors.Vector;
      end record;

end Leander.Syntax.Modules;
