private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Leander.Types.Bindings;
with Leander.Types.Trees;

with Leander.Core.Bindings;
with Leander.Core.Trees;

package Leander.Environments is

   type Environment is tagged private;

   procedure Create
     (Env : in out Environment'Class;
      Name : String);

   function Has_Expression_Binding
     (Env  : Environment;
      Name : String)
      return Boolean;

   function Expression_Binding
     (Env : Environment;
      Name : String)
      return Leander.Core.Trees.Tree_Type'Class
     with Pre => Env.Has_Expression_Binding (Name);

   function Has_Constructor_Binding
     (Env  : Environment;
      Name : String)
      return Boolean;

   function Constructor_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Types.Bindings.Constructor_Binding'Class
     with Pre => Env.Has_Constructor_Binding (Name);

   function Has_Type_Constructor_Binding
     (Env  : Environment;
      Name : String)
      return Boolean;

   function Type_Constructor_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Types.Bindings.Type_Binding'Class
     with Pre => Env.Has_Type_Constructor_Binding (Name);

   procedure Insert_Value
     (Env   : in out Environment;
      Name  : String;
      Value : Leander.Core.Trees.Tree_Type);

   procedure Declare_Data_Type
     (Env       : in out Environment;
      Name      : String;
      Data_Type : Leander.Types.Trees.Tree_Type);

   procedure Declare_New_Type
     (Env       : in out Environment;
      Name      : String;
      New_Type  : Leander.Types.Trees.Tree_Type);

   procedure Declare_Primitive_Type
     (Env            : in out Environment;
      Name           : String;
      Primitive_Type : Leander.Types.Trees.Tree_Type);

   procedure Insert_Constructor
     (Env       : in out Environment;
      Type_Name : String;
      Name      : String;
      Con_Type  : Leander.Types.Trees.Tree_Type);

   procedure Annotate
     (Env  : Leander.Environments.Environment'Class);

   procedure Import_Names
     (To   : in out Environment'Class;
      From : Environment'Class);

private

   package Export_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Environment_Record;

   type Environment_Access is access Environment_Record;

   type Environment_Record is
      record
         Name         : Ada.Strings.Unbounded.Unbounded_String;
         Exports      : Export_Lists.List;
         Export_All   : Boolean := True;
         Values       : Leander.Core.Bindings.Binding_List;
         Constructors : Leander.Types.Bindings.Constructor_Binding_List;
         Types        : Leander.Types.Bindings.Type_Binding_List;
      end record;

   type Environment is tagged
      record
         Local  : Environment_Access;
         Global : Environment_Access;
      end record;

   function Has_Expression_Binding
     (Env  : Environment;
      Name : String)
      return Boolean
   is (Env.Local.Values.Has_Binding (Name));

   function Expression_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Core.Trees.Tree_Type'Class
   is (Env.Local.Values.Binding (Name));

   function Has_Constructor_Binding
     (Env  : Environment;
      Name : String)
      return Boolean
   is (Env.Local.Constructors.Has_Binding (Name));

   function Constructor_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Types.Bindings.Constructor_Binding'Class
   is (Env.Local.Constructors.Binding (Name));

   function Has_Type_Constructor_Binding
     (Env  : Environment;
      Name : String)
      return Boolean
   is (Env.Local.Types.Has_Binding (Name));

   function Type_Constructor_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Types.Bindings.Type_Binding'Class
   is (Env.Local.Types.Binding (Name));

end Leander.Environments;
