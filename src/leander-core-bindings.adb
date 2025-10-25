with Leander.Allocator;

package body Leander.Core.Bindings is

   type Variable_Reference is access all Instance;

   package Allocator is
     new Leander.Allocator ("bindings", Instance, Variable_Reference);

   function Allocate
     (This : Instance'Class)
      return Reference
   is (Reference (Allocator.Allocate (Instance (This))));

   ----------------------
   -- Implicit_Binding --
   ----------------------

   function Implicit_Binding
     (Name : Varid;
      Alts : Leander.Core.Alts.Reference_Array)
      return Reference
   is
   begin
      return Allocate (Instance'(Alts'Length, Name, Alts, null));
   end Implicit_Binding;

   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      Allocator.Prune;
   end Prune;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is
      function Show_Alts (Index : Positive) return String;

      ---------------
      -- Show_Alts --
      ---------------

      function Show_Alts (Index : Positive) return String is
         Img : constant String := This.Alts (Index).Show;
      begin
         if Index < This.Alt_Count then
            return Img & ";" & Show_Alts (Index + 1);
         else
            return Img;
         end if;
      end Show_Alts;

   begin
      return Core.To_String (This.Name) & "=" & Show_Alts (1);
   end Show;

end Leander.Core.Bindings;
