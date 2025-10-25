with Leander.Allocator;

package body Leander.Core.Binding_Groups is

   type Variable_Reference is access all Instance;

   package Allocator is
     new Leander.Allocator ("bindings", Instance, Variable_Reference);

   function Allocate
     (This : Instance'Class)
      return Reference
   is (Reference (Allocator.Allocate (Instance (This))));

   ---------------------------
   -- Add_Explicit_Bindings --
   ---------------------------

   procedure Add_Explicit_Bindings
     (This : in out Instance_Builder'Class;
      Bindings : Leander.Core.Bindings.Reference_Array)
   is
      pragma Assert (This.Item.Explicit_Bindings.Is_Empty);
   begin
      This.Item.Explicit_Bindings.Append (Bindings);
   end Add_Explicit_Bindings;

   ---------------------------
   -- Add_Implicit_Bindings --
   ---------------------------

   procedure Add_Implicit_Bindings
     (This     : in out Instance_Builder'Class;
      Bindings : Leander.Core.Bindings.Reference_Array)
   is
   begin
      This.Item.Implicit_Bindings.Append (Bindings);
   end Add_Implicit_Bindings;

   -----------------------
   -- Get_Binding_Group --
   -----------------------

   function Get_Binding_Group
     (This : Instance_Builder'Class)
      return Reference
   is
   begin
      return Allocate (This.Item);
   end Get_Binding_Group;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is
      package String_Lists is
        new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
      Images : String_Lists.List;

      procedure Add (List : Binding_Array_Lists.List);
      function Join (Position : String_Lists.Cursor) return String;

      ---------
      -- Add --
      ---------

      procedure Add (List : Binding_Array_Lists.List) is
      begin
         for Element of List loop
            for Binding of Element loop
               Images.Append (Binding.Show);
            end loop;
         end loop;
      end Add;

      ----------
      -- Join --
      ----------

      function Join (Position : String_Lists.Cursor) return String is
         use String_Lists;
      begin
         if not Has_Element (Position) then
            return "";
         elsif not Has_Element (Next (Position)) then
            return Element (Position);
         else
            return Element (Position) & ";" & Join (Next (Position));
         end if;
      end Join;

   begin
      Add (This.Explicit_Bindings);
      Add (This.Implicit_Bindings);

      return Join (Images.First);
   end Show;

end Leander.Core.Binding_Groups;
