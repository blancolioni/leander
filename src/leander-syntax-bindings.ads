private with Ada.Containers.Doubly_Linked_Lists;

with Leander.Core.Binding_Groups;
limited with Leander.Syntax.Expressions;
with Leander.Syntax.Patterns;

package Leander.Syntax.Bindings is

   subtype Parent is Leander.Syntax.Instance;
   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   procedure Add_Binding
     (This : in out Instance;
      Pat  : not null access constant Patterns.Instance'Class;
      Expr : not null access constant Expressions.Instance'Class);

   function To_Core
     (This : Instance)
      return Leander.Core.Binding_Groups.Reference;

   function Empty return Reference;

private

   type Expression_Reference is
     access constant Leander.Syntax.Expressions.Instance'Class;

   type Binding_Record is
      record
         Pat : Patterns.Reference;
         Expr : Expression_Reference;
      end record;

   package Binding_Record_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Binding_Record);

   type Name_Binding is
      record
         Name      : Leander.Core.Name_Id;
         Equations : Binding_Record_Lists.List;
      end record;

   package Name_Binding_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Name_Binding);

   type Instance is new Parent with
      record
         Bindings : Name_Binding_Lists.List;
      end record;

end Leander.Syntax.Bindings;
