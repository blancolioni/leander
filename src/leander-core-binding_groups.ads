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

   function Varids
     (This : Instance'Class)
      return Varid_Array;

   function Implicit_Group
     (This : Instance'Class;
      Name : Varid)
      return Leander.Core.Bindings.Reference_Array;
   --  The implicit bindings inferred together with Name, Name included:
   --  each uses the others monomorphically, applying no dictionaries.
   --  Empty if Name is not an implicit binding of This.

   function Has_Reference
     (This : Instance'Class;
      To   : Varid)
      return Boolean;

   function Component_Count (This : Instance'Class) return Natural;

   function Component
     (This  : Instance'Class;
      Index : Positive)
      return Leander.Core.Bindings.Reference_Array
     with Pre => Index <= This.Component_Count;

   function Component_Of
     (This : Instance'Class;
      Name : Varid)
      return Natural;
   --  The strongly connected components of the dependency graph over all
   --  of This's bindings, explicit and implicit alike, numbered so that
   --  everything a component refers to has a lower number.  This is the
   --  order the bindings must be compiled in, rather than inferred in: a
   --  binding refers to an explicit one by name whatever its type, so a
   --  component of several bindings is a cycle of references that only a
   --  recursive tuple can tie (see Expressions.Recursive_Component).
   --  Component_Of is 0 for a name not bound by This.

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

   procedure Report;

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
         Components        : Binding_Array_Lists.List;
      end record;

   overriding function Show (This : Instance) return String;

   function Component_Count (This : Instance'Class) return Natural
   is (Natural (This.Components.Length));

   type Instance_Builder is tagged
      record
         Item : Instance;
      end record;

end Leander.Core.Binding_Groups;
