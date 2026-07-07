with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with System;

package body Leander.Allocator is

   package Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Reference);

   Protected_List : Reference_Lists.List;
   Allocated_List : Reference_Lists.List;

   --  All-time statistics (never reset), in nodes and bytes.  Live totals
   --  are the all-time allocated minus all-time freed.
   Alloc_Count : Natural := 0;
   Free_Count  : Natural := 0;
   Alloc_Bytes : Long_Long_Integer := 0;
   Free_Bytes  : Long_Long_Integer := 0;

   function Byte_Size (This : Reference) return Long_Long_Integer
   is (Long_Long_Integer (This.all'Size / System.Storage_Unit));

   --------------
   -- Allocate --
   --------------

   function Allocate (This : Instance) return Reference is
      V : constant Reference := new Instance'(This);
   begin
      Allocated_List.Append (V);
      Alloc_Count := Alloc_Count + 1;
      Alloc_Bytes := Alloc_Bytes + Byte_Size (V);
      return V;
   end Allocate;

   -------------
   -- Protect --
   -------------

   procedure Protect (This : Reference) is
   begin
      Protected_List.Append (This);
   end Protect;

   -----------
   -- Prune --
   -----------

   procedure Prune is
      procedure Free is
        new Ada.Unchecked_Deallocation (Instance, Reference);
   begin
      for Ref of Allocated_List loop
         if not Protected_List.Contains (Ref) then
            Free_Count := Free_Count + 1;
            Free_Bytes := Free_Bytes + Byte_Size (Ref);
            Free (Ref);
         end if;
      end loop;
      Allocated_List.Clear;
   end Prune;

   ------------
   -- Report --
   ------------

   procedure Report is
      use Ada.Text_IO;
   begin
      Put_Line (Name & " allocator:");
      Put_Line ("  live nodes    :" & Natural'Image (Alloc_Count - Free_Count));
      Put_Line ("  live bytes    :" & Long_Long_Integer'Image
                (Alloc_Bytes - Free_Bytes));
      Put_Line ("  total allocs  :" & Natural'Image (Alloc_Count));
      Put_Line ("  total frees   :" & Natural'Image (Free_Count));
   end Report;

end Leander.Allocator;
