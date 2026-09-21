with Leander.Calculus;
with Leander.Core.Binding_Groups;
with Leander.Core.Predicates;
with Leander.Core.Schemes;
with Leander.Core.Type_Classes;
with Leander.Core.Type_Env;
with Leander.Core.Type_Instances;
with Leander.Core.Types;
with Leander.Data_Types;
with Leander.Names;

package Leander.Environment is

   type Abstraction is interface
     and Leander.Core.Type_Classes.Class_Environment;

   type Reference is access all Abstraction'Class;

   type Type_Class_Array is
     array (Positive range <>) of Leander.Core.Type_Classes.Reference;

   type Data_Type_Array is
     array (Positive range <>) of Leander.Data_Types.Reference;

   type Element_Class is
     (Type_Constructor, Constructor, Variable_Binding,
      Class_Binding);

   function Name (This : Abstraction) return String is abstract;

   function Exists
     (This  : Abstraction;
      Name  : Leander.Names.Leander_Name;
      Class : Element_Class)
      return Boolean
      is abstract;

   function Type_Env
     (This : Abstraction)
      return Leander.Core.Type_Env.Reference
      is abstract;

   function Constructor
     (This : Abstraction;
      Name : Leander.Names.Leander_Name)
      return Leander.Core.Schemes.Reference
      is abstract
     with Pre'Class => This.Exists (Name, Constructor);

   function Constructor
     (This : Abstraction'Class;
      Name : String)
      return Leander.Core.Schemes.Reference
   is (This.Constructor (Leander.Names.To_Leander_Name (Name)));

   function Constructor
     (This : Abstraction;
      Name : Leander.Names.Leander_Name)
      return Leander.Calculus.Tree
      is abstract
     with Pre'Class => This.Exists (Name, Constructor);

   function Con_Data_Type
     (This : Abstraction;
      Id   : Leander.Core.Conid)
      return Leander.Data_Types.Reference
      is abstract
     with Pre'Class => This.Exists (Leander.Names.Leander_Name (Id),
                                    Constructor);

   function Data_Type
     (This : Abstraction;
      Id   : Leander.Core.Conid)
      return Leander.Data_Types.Reference
      is abstract
     with Pre'Class => This.Exists (Leander.Names.Leander_Name (Id),
                                    Type_Constructor);

   procedure Bindings
     (This   : in out Abstraction;
      Groups : Leander.Core.Binding_Groups.Reference)
   is abstract;

   procedure Data_Type
     (This   : in out Abstraction;
      DT     : Leander.Data_Types.Reference)
   is abstract;

   procedure Type_Class
     (This  : in out Abstraction;
      Class : Leander.Core.Type_Classes.Reference)
   is abstract;

   procedure Type_Instance
     (This          : in out Abstraction;
      Class_Id      : Leander.Core.Conid;
      Constraints   : Leander.Core.Predicates.Predicate_Array;
      Instance_Type : Leander.Core.Types.Reference;
      Bindings      : Leander.Core.Binding_Groups.Reference)
   is abstract;

   type Import_Visibility is (All_Names, Only_Names, Except_Names, No_Names);
   --  What an import declaration makes available unqualified: everything
   --  (a plain import), just a list, everything but a list (hiding), or
   --  nothing at all (qualified).

   procedure Import
     (This    : in out Abstraction;
      Env     : not null access Abstraction'Class;
      Mode    : Import_Visibility := All_Names;
      Names   : Leander.Names.Name_Array := [])
   is abstract;
   --  Bring Env into scope. Env's own declarations always arrive under
   --  their canonical names, so that they can be reached with a qualifier
   --  whatever the import says, and additionally under their bare names
   --  when Mode and Names make them visible unqualified. Nothing is ever
   --  removed: a name this import does not make visible is simply never
   --  inserted bare, which is what makes writing it bare fail.

   procedure Foreign_Import
     (This         : in out Abstraction;
      Name         : String;
      Foreign_Name : String;
      Signature    : Leander.Core.Types.Reference)
   is abstract;

   procedure Set_Scheme
     (This   : in out Abstraction;
      Name   : String;
      Scheme : Leander.Core.Schemes.Reference)
   is abstract;
   --  Record Name's already-known Scheme (e.g. decoded from a loaded
   --  module image's Annotations) without touching Values -- unlike
   --  Foreign_Import, this makes no claim about how Name's value is
   --  bound, only what its type is. Get_Bound_Calculus is unaffected: it
   --  still compiles Name from Bindings on first use if nothing else
   --  (a pre-loaded Skit binding) short-circuits that first.

   procedure Elaborate
     (This : in out Abstraction)
   is abstract;

   function Variable_Binding_Exists
     (This : Abstraction;
      Name : String)
      return Boolean
      is abstract;

   function Value_Names
     (This : Abstraction)
      return Leander.Names.Name_Array
      is abstract;
   --  Every top-level name bound in This (Values), for module dumping. This
   --  is deliberately narrower than Type_Env's Ids: Type_Env accumulates
   --  every variable inference ever assigned a type -- including local
   --  lambda- and pattern-bound names from within a binding's own body --
   --  not just this module's public top-level bindings.

   function Declares
     (This : Abstraction;
      Name : String)
      return Boolean
      is abstract;
   --  Whether Name is one of this module's own declarations rather than
   --  something it inherited through an Import. The maps themselves
   --  cannot answer this: Import copies into them, so by the time anyone
   --  asks, an inherited name looks exactly like a declared one.

   procedure Start_Exports (This : in out Abstraction) is abstract;
   procedure Add_Export
     (This : in out Abstraction;
      Name : String)
      is abstract;
   --  Record that this module has an export list, and add one name to it.
   --  A module that never calls Start_Exports exports everything it
   --  declares, which is Haskell's default for a module with no list.
   --  These must be called only once the declarations are in: "T(..)"
   --  cannot name its constructors, and an export naming something the
   --  module never declared cannot be spotted, any earlier.

   function Is_Exported
     (This : Abstraction;
      Name : String)
      return Boolean
      is abstract;
   --  Whether another module may see Name. Always False for a machine
   --  primitive, which is an FFI symbol rather than a Haskell entity, and
   --  always True for built-in syntax and for the synthetic names that
   --  dictionary passing resolves by string.

   function Exports_Everything (This : Abstraction) return Boolean
      is abstract;
   --  True when this module wrote no export list, and so exports all of
   --  its own declarations. Distinct from an empty list, which exports
   --  nothing.

   function Export_Names
     (This : Abstraction)
      return Leander.Names.Name_Array
      is abstract;
   --  The names the module's export list mentions, once expanded. Empty
   --  when Exports_Everything, which is not the same as an empty list.

   function Canonical_Name
     (This : Abstraction;
      Name : String)
      return String
      is abstract;
   --  The key another module sees Name under once it has imported this
   --  one. That is Name itself for the Prelude, for built-in syntax and
   --  for synthetic names, and "<module>." & Name otherwise.

   function Local_Name
     (This : Abstraction;
      Name : String)
      return String
      is abstract;
   --  The inverse of Canonical_Name: strip this module's own prefix from
   --  Name when it carries one, so that an importer holding a canonical
   --  key can ask the exporting module about it under the name that
   --  module knows. Names belonging to anything else pass through.

   function Declared_Names
     (This : Abstraction)
      return Leander.Names.Name_Array
      is abstract;
   --  Every name this module declares itself, across all namespaces.

   function Own_Classes
     (This : Abstraction)
      return Type_Class_Array
      is abstract;

   function Own_Data_Types
     (This : Abstraction)
      return Data_Type_Array
      is abstract;

   function Own_Instances
     (This : Abstraction)
      return Leander.Core.Type_Instances.Reference_Array
      is abstract;
   --  This module's own classes/data types/instance facts (not ones
   --  inherited via Import), for Dump_Module to encode into a module's
   --  .skix image alongside its ordinary value exports.

   function Get_Bound_Calculus
     (This             : in out Abstraction;
      Variable_Binding : String)
      return Leander.Calculus.Tree
      is abstract
     with Pre'Class => This.Variable_Binding_Exists (Variable_Binding);

   function New_Environment (Name : String) return Reference;
   function Boot_Environment return Reference;

end Leander.Environment;
