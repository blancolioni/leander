private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Hash;

with Leander.Core.Bindings;
with Leander.Types.Trees;
with Leander.Core.Trees;

package Leander.Types.Class_Constraints is

   type Class_Constraint is new Type_Constraint with private;

   procedure Create
     (Class : in out Class_Constraint'Class);

   procedure Set_Constraint
     (Class : in out Class_Constraint'Class;
      Name  : String;
      Tyvar : String);

   procedure Add_Context
     (Class   : in out Class_Constraint'Class;
      Context : Class_Constraint'Class);

   procedure Add_Method
     (Class     : in out Class_Constraint'Class;
      Name      : String;
      Signature : Leander.Types.Trees.Tree_Type;
      Default   : Leander.Core.Trees.Tree_Type);

   function Type_Variable
     (Class : Class_Constraint'Class)
      return Leander.Types.Trees.Tree_Type;

   type Class_Bindings is tagged private;

   function Has_Binding
     (Bindings : Class_Bindings;
      Name     : String)
      return Boolean;

   function Binding
     (Bindings : Class_Bindings;
      Name     : String)
      return Class_Constraint'Class
     with Pre => Bindings.Has_Binding (Name);

   procedure Insert
     (Bindings : in out Class_Bindings;
      Name     : String;
      Binding  : Class_Constraint'Class)
     with Pre => not Bindings.Has_Binding (Name),
     Post => Bindings.Has_Binding (Name);

private

   type Class_Record;

   type Class_Constraint is new Type_Constraint with
      record
         Class_Body : access Class_Record;
      end record;

   overriding function Show
     (Constraint : Class_Constraint)
      return String;

   overriding function Is_Subset_Of
     (Subset : Class_Constraint;
      Superset : Class_Constraint)
      return Boolean;

   package Class_Binding_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Class_Constraint'Class,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   type Class_Bindings is tagged
      record
         Map : Class_Binding_Maps.Map;
      end record;

end Leander.Types.Class_Constraints;
