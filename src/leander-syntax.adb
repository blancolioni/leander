with Leander.Allocator;

package body Leander.Syntax is

   type Variable_Reference is access all Instance'Class;

   package Allocator is
     new Leander.Allocator ("syntax",
                            Instance'Class, Variable_Reference);

   --------------
   -- Allocate --
   --------------

   function Allocate
     (This : Instance'Class)
      return Reference
   is
   begin
      return Reference (Allocator.Allocate (This));
   end Allocate;

   -------------
   -- Protect --
   -------------

   procedure Protect (This : not null Reference) is
   begin
      Allocator.Protect (Variable_Reference (This));
   end Protect;

   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      Allocator.Prune;
   end Prune;

end Leander.Syntax;
