private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with SK.Machine;

with Leander.Types.Bindings;
with Leander.Types.Class_Constraints;
with Leander.Types.Trees;

with Leander.Core.Bindings;
with Leander.Core.Trees;

package Leander.Environments is

   type Environment is tagged private;

   procedure Create
     (Env  : in out Environment'Class;
      Name : String);

   procedure Create_Temporary_Environment
     (Env    : in out Environment'Class;
      Parent : Environment'Class;
      Name   : String);

   function Has_Local_Binding
     (Env  : Environment;
      Name : String)
      return Boolean;

   function Local_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Core.Trees.Tree_Type
     with Pre => Env.Has_Local_Binding (Name);

   function Has_Local_Signature
     (Env  : Environment;
      Name : String)
      return Boolean;

   function Local_Signature
     (Env  : Environment;
      Name : String)
      return Leander.Types.Trees.Tree_Type
     with Pre => Env.Has_Local_Signature (Name);

   procedure Scan_Local_Bindings
     (Env     : Environment'Class;
      Process : not null access
        procedure (Name : String;
                   Tree : Leander.Core.Trees.Tree_Type));

   procedure Scan_Local_Bindings
     (Env     : Environment'Class;
      Process : not null access
        procedure (Name : String;
                   Tree : Leander.Core.Trees.Tree_Type;
                   Signature : Leander.Types.Trees.Tree_Type));

   procedure Scan_Local_Type_Bindings
     (Env     : Environment'Class;
      Process : not null access
        procedure (Name : String;
                   Tree : Leander.Types.Bindings.Type_Binding'Class));

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

   function Has_Type_Variable_Binding
     (Env  : Environment;
      Name : String)
      return Boolean;

   function Type_Variable_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Types.Trees.Tree_Type
     with Pre => Env.Has_Type_Variable_Binding (Name);

   procedure Add_Type_Assertion
     (Env       : Environment;
      Name      : String;
      Assertion : Leander.Types.Type_Assertion'Class);

   function Has_Class_Binding
     (Env  : Environment;
      Name : String)
      return Boolean;

   function Class_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Types.Class_Constraints.Class_Constraint'Class
     with Pre => Env.Has_Class_Binding (Name);

   procedure Scan_Local_Class_Bindings
     (Env     : Environment'Class;
      Process : not null access
        procedure (Binding : Types.Class_Constraints.Class_Constraint'Class));

   procedure Insert_Value
     (Env   : in out Environment;
      Name  : String;
      Value : Leander.Core.Trees.Tree_Type);

   procedure Insert_Type_Variable
     (Env   : in out Environment;
      Name  : String;
      Value : Leander.Types.Trees.Tree_Type);

   procedure Insert_Signature
     (Env   : in out Environment;
      Name  : String;
      Value : Leander.Types.Trees.Tree_Type);

   procedure Insert_Foreign_Import
     (Env          : in out Environment;
      Name         : String;
      Foreign_Name : String;
      Signature    : Leander.Types.Trees.Tree_Type);

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
      Con_Type  : Leander.Types.Trees.Tree_Type;
      Con_Arity : Natural);

   procedure Insert_Class_Binding
     (Env   : Environment;
      Name  : String;
      Class : Leander.Types.Class_Constraints.Class_Constraint'Class);

   procedure Annotate
     (Env  : Leander.Environments.Environment'Class);

   procedure Compile
     (Env     : Leander.Environments.Environment;
      Machine : SK.Machine.SK_Machine);

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
         Classes      : Leander.Types.Class_Constraints.Class_Bindings;
      end record;

   type Environment is tagged
      record
         Local  : Environment_Access;
         Global : Environment_Access;
      end record;

   function Has_Local_Binding
     (Env  : Environment;
      Name : String)
      return Boolean
   is (Env.Local.Values.Has_Binding (Name));

   function Local_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Core.Trees.Tree_Type
   is (Env.Local.Values.Binding (Name));

   function Has_Local_Signature
     (Env  : Environment;
      Name : String)
      return Boolean
   is (Env.Local.Values.Has_Signature (Name));

   function Local_Signature
     (Env  : Environment;
      Name : String)
      return Leander.Types.Trees.Tree_Type
   is (Env.Local.Values.Signature (Name));

   function Has_Expression_Binding
     (Env  : Environment;
      Name : String)
      return Boolean
   is (Env.Has_Local_Binding (Name)
       or else (Env.Global /= null
                and then Env.Global.Values.Has_Binding (Name)));

   function Expression_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Core.Trees.Tree_Type'Class
   is (if Env.Has_Local_Binding (Name) then Env.Local_Binding (Name)
       else Env.Global.Values.Binding (Name));

   function Has_Constructor_Binding
     (Env  : Environment;
      Name : String)
      return Boolean
   is (Env.Local.Constructors.Has_Binding (Name)
       or else (Env.Global /= null
                and then Env.Global.Constructors.Has_Binding (Name)));

   function Constructor_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Types.Bindings.Constructor_Binding'Class
   is (Env.Local.Constructors.Binding (Name));

   function Has_Type_Constructor_Binding
     (Env  : Environment;
      Name : String)
      return Boolean
   is (Env.Local.Types.Has_Binding (Name)
       or else (Env.Global /= null
                and then Env.Global.Types.Has_Binding (Name)));

   function Type_Constructor_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Types.Bindings.Type_Binding'Class
   is (if Env.Local.Types.Has_Binding (Name)
       then Env.Local.Types.Binding (Name)
       else Env.Global.Types.Binding (Name));

   function Has_Type_Variable_Binding
     (Env  : Environment;
      Name : String)
      return Boolean
   is (Env.Local.Types.Has_Binding (Name));

   function Type_Variable_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Types.Trees.Tree_Type
   is (Env.Local.Types.Variable_Binding (Name));

   function Has_Class_Binding
     (Env  : Environment;
      Name : String)
      return Boolean
   is (Env.Local.Classes.Has_Binding (Name)
       or else (Env.Global /= null
                and then Env.Global.Classes.Has_Binding (Name)));

   function Class_Binding
     (Env  : Environment;
      Name : String)
      return Leander.Types.Class_Constraints.Class_Constraint'Class
   is (if Env.Local.Classes.Has_Binding (Name)
       then Env.Local.Classes.Binding (Name)
       else Env.Global.Classes.Binding (Name));

end Leander.Environments;
