with Leander.Calculus;
with Leander.Core.Binding_Groups;
with Leander.Core.Schemes;
with Leander.Core.Type_Classes;
with Leander.Core.Type_Env;
with Leander.Core.Types;
with Leander.Data_Types;
with Leander.Names;
--  with Leander.Types.Bindings;

package Leander.Environment is

   type Abstraction is interface
     and Leander.Calculus.Calculus_Environment;

   type Reference is access all Abstraction'Class;

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
     (This : in out Abstraction;
      Class : Leander.Core.Type_Classes.Reference)
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

   procedure Elaborate
     (This : in out Abstraction)
   is abstract;

   function New_Environment (Name : String) return Reference;

end Leander.Environment;
