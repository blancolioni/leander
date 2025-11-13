private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Doubly_Linked_Lists;

with Leander.Core.Binding_Groups;
with Leander.Names;
limited with Leander.Syntax.Expressions;
with Leander.Syntax.Patterns;
with Leander.Core.Predicates;
with Leander.Syntax.Qualified_Types;

package Leander.Syntax.Bindings is

   subtype Parent is Leander.Syntax.Instance;
   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   procedure Add_Binding
     (This      : in out Instance;
      Loc       : Source.Source_Location;
      Name      : String;
      Pats      : Patterns.Reference_Array;
      Expr      : not null access constant Expressions.Instance'Class);

   procedure Add_Type
     (This      : in out Instance;
      Loc       : Source.Source_Location;
      Name      : String;
      Type_Expr : Leander.Syntax.Qualified_Types.Reference);

   function To_Core
     (This          : Instance)
      return Leander.Core.Binding_Groups.Reference;

   function Empty
     (Context    : Core.Declaration_Context := Core.Binding_Context;
      Predicates : Leander.Core.Predicates.Predicate_Array := [])
      return Reference;

private

   type Expression_Reference is
     access constant Leander.Syntax.Expressions.Instance'Class;

   type Binding_Record (Pat_Count : Natural) is
      record
         Pats : Patterns.Reference_Array (1 .. Pat_Count);
         Expr : Expression_Reference;
      end record;

   package Binding_Record_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Binding_Record);

   type Name_Binding is
      record
         Name      : Leander.Names.Leander_Name;
         Equations : Binding_Record_Lists.List;
      end record;

   package Name_Binding_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Name_Binding);

   type Type_Binding is
      record
         Name      : Leander.Names.Leander_Name;
         Type_Expr : Leander.Syntax.Qualified_Types.Reference;
      end record;

   package Type_Binding_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Type_Binding);

   package Predicate_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Leander.Core.Predicates.Instance,
        Leander.Core.Predicates."=");

   type Instance is new Parent with
      record
         Context     : Core.Declaration_Context;
         Predicates  : Predicate_Lists.List;
         Bindings    : Name_Binding_Lists.List;
         Types       : Type_Binding_Lists.List;
      end record;

end Leander.Syntax.Bindings;
