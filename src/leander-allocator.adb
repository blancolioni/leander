with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Leander.Allocator is

   package Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Reference);

   Protected_List : Reference_Lists.List;
   Allocated_List : Reference_Lists.List;

   --------------
   -- Allocate --
   --------------

   function Allocate (This : Instance) return Reference is
      V : constant Reference := new Instance'(This);
   begin
      Allocated_List.Append (V);
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
      Total  : Natural := 0;
      Pruned : Natural := 0;
   begin
      for Ref of Allocated_List loop
         Total := Total + 1;
         if not Protected_List.Contains (Ref) then
            Free (Ref);
            Pruned := Pruned + 1;
         end if;
      end loop;
      Allocated_List.Clear;
      Ada.Text_IO.Put_Line
        (Name & ": total" & Total'Image
         & "; pruned" & Pruned'Image
         & "; kept" & Natural'Image (Total - Pruned));
   end Prune;

end Leander.Allocator;
