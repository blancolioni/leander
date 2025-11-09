with Ada.Containers.Doubly_Linked_Lists;

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

   -------------------
   -- Has_Reference --
   -------------------

   function Has_Reference
     (This : Instance'Class;
      To   : Varid)
      return Boolean
   is
      function Exists_In (List : Binding_Array_Lists.List) return Boolean;

      ---------------
      -- Exists_In --
      ---------------

      function Exists_In (List : Binding_Array_Lists.List) return Boolean is
      begin
         for Arr of List loop
            for B of Arr loop
               if B.Has_Reference (To) then
                  return True;
               end if;
            end loop;
         end loop;
         return False;
      end Exists_In;

   begin
      return Exists_In (This.Explicit_Bindings)
        or else Exists_In (This.Implicit_Bindings);
   end Has_Reference;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (This : Instance'Class;
      Name : Leander.Names.Leander_Name)
      return Leander.Core.Bindings.Reference
   is
      function Find
        (List : Binding_Array_Lists.List)
         return Leander.Core.Bindings.Reference;

      ----------
      -- Find --
      ----------

      function Find
        (List : Binding_Array_Lists.List)
         return Leander.Core.Bindings.Reference
      is
      begin
         for Arr of List loop
            for B of Arr loop
               if B.Name = Varid (Name) then
                  return B;
               end if;
            end loop;
         end loop;
         return null;
      end Find;

      use type Leander.Core.Bindings.Reference;
      B : Leander.Core.Bindings.Reference :=
            Find (This.Explicit_Bindings);
   begin
      if B = null then
         B := Find (This.Implicit_Bindings);
      end if;
      return B;
   end Lookup;

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

   ------------
   -- Varids --
   ------------

   function Varids
     (This : Instance'Class)
      return Varid_Array
   is
      package Varid_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Varid);
      Varid_List : Varid_Lists.List;

      procedure Add (List : Binding_Array_Lists.List) ;

      ---------
      -- Add --
      ---------

      procedure Add (List : Binding_Array_Lists.List) is
      begin
         for Element of List loop
            for Binding of Element loop
               Varid_List.Append (Binding.Name);
            end loop;
         end loop;
      end Add;

   begin
      Add (This.Explicit_Bindings);
      Add (This.Implicit_Bindings);
      return [for Id of Varid_List => Id];
   end Varids;

end Leander.Core.Binding_Groups;
