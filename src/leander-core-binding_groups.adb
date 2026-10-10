with Ada.Containers.Doubly_Linked_Lists;

with Leander.Allocator;
with Leander.Core.Bindings.Dependencies;

package body Leander.Core.Binding_Groups is

   type Variable_Reference is access all Instance;

   package Allocator is
     new Leander.Allocator ("binding_groups", Instance, Variable_Reference);

   function Allocate
     (This : Instance'Class)
      return Reference
   is (Reference (Allocator.Allocate (Instance (This))));

   ------------
   -- Report --
   ------------

   procedure Report is
   begin
      Allocator.Report;
   end Report;

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
      Item      : Instance := This.Item;
      Ids       : constant Varid_Array := Item.Varids;
      Bs        : constant Leander.Core.Bindings.Reference_Array :=
                    [for Id of Ids =>
                       Item.Lookup (Leander.Names.Leander_Name (Id))];
      Component : constant
        Leander.Core.Bindings.Dependencies.Component_Array :=
          Leander.Core.Bindings.Dependencies.Components (Bs);
   begin
      for C in 1 .. Leander.Core.Bindings.Dependencies.Component_Count
                      (Component)
      loop
         declare
            Members : Leander.Core.Bindings.Reference_Array (Bs'Range);
            Count   : Natural := 0;
         begin
            for I in Bs'Range loop
               if Component (I) = C then
                  Count := Count + 1;
                  Members (Count) := Bs (I);
               end if;
            end loop;
            Item.Components.Append (Members (1 .. Count));
         end;
      end loop;
      return Allocate (Item);
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

   ---------------
   -- Component --
   ---------------

   function Component
     (This  : Instance'Class;
      Index : Positive)
      return Leander.Core.Bindings.Reference_Array
   is
      Position : Binding_Array_Lists.Cursor := This.Components.First;
   begin
      for I in 2 .. Index loop
         Binding_Array_Lists.Next (Position);
      end loop;
      return Binding_Array_Lists.Element (Position);
   end Component;

   ------------------
   -- Component_Of --
   ------------------

   function Component_Of
     (This : Instance'Class;
      Name : Varid)
      return Natural
   is
      Index : Natural := 0;
   begin
      for Members of This.Components loop
         Index := Index + 1;
         for B of Members loop
            if B.Name = Name then
               return Index;
            end if;
         end loop;
      end loop;
      return 0;
   end Component_Of;

   --------------------
   -- Implicit_Group --
   --------------------

   function Implicit_Group
     (This : Instance'Class;
      Name : Varid)
      return Leander.Core.Bindings.Reference_Array
   is
   begin
      for Group of This.Implicit_Bindings loop
         for B of Group loop
            if B.Name = Name then
               return Group;
            end if;
         end loop;
      end loop;
      return [];
   end Implicit_Group;

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
