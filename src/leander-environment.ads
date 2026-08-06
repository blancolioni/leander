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

   procedure Import
     (This : in out Abstraction;
      Env  : not null access Abstraction'Class)
   is abstract;

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
