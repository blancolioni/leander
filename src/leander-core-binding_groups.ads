private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Leander.Core.Bindings;
with Leander.Core.Types;
with Leander.Showable;

package Leander.Core.Binding_Groups is

   type Instance is
     new Leander.Showable.Abstraction
   with private;

   type Reference is access constant Instance'Class;

   function Lookup
     (This : Instance'Class;
      Name : Leander.Names.Leander_Name)
      return Leander.Core.Bindings.Reference;

   type Instance_Builder is tagged private;

   procedure Add_Explicit_Bindings
     (This : in out Instance_Builder'Class;
      Bindings : Leander.Core.Bindings.Reference_Array);

   procedure Add_Implicit_Bindings
     (This     : in out Instance_Builder'Class;
      Bindings : Leander.Core.Bindings.Reference_Array);

   function Get_Binding_Group
     (This : Instance_Builder'Class)
      return Reference;

private

   type Nullable_Type_Reference is
     access all Leander.Core.Types.Instance'Class;

   package Binding_Array_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Leander.Core.Bindings.Reference_Array,
        Leander.Core.Bindings."=");

   type Instance is
     new Leander.Showable.Abstraction with
      record
         Explicit_Bindings : Binding_Array_Lists.List;
         Implicit_Bindings : Binding_Array_Lists.List;
      end record;

   overriding function Show (This : Instance) return String;

   type Instance_Builder is tagged
      record
         Item : Instance;
      end record;

end Leander.Core.Binding_Groups;
