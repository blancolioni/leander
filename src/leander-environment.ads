with Leander.Core.Binding_Groups;
with Leander.Core.Kinds;
with Leander.Core.Schemes;
with Leander.Core.Type_Env;
with Leander.Core.Types;
with Leander.Names;
--  with Leander.Types.Bindings;

package Leander.Environment is

   type Abstraction is interface;
   type Reference is access all Abstraction'Class;

   type Element_Class is
     (Type_Constructor, Constructor, Variable_Binding);

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

   procedure Bindings
     (This   : in out Abstraction;
      Groups : Leander.Core.Binding_Groups.Reference)
   is abstract;

   procedure Data_Type
     (This   : in out Abstraction;
      Tycon  : Leander.Core.Types.Reference;
      Kind   : Leander.Core.Kinds.Kind;
      Cons   : Leander.Core.Type_Env.Reference)
   is abstract;

   procedure Elaborate
     (This : in out Abstraction)
   is abstract;

   function New_Environment (Name : String) return Reference;

end Leander.Environment;
