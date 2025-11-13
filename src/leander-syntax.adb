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
     (This : Instance)
      return Reference
   is
   begin
      return Reference (Allocator.Allocate (This));
   end Allocate;

   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      Allocator.Prune;
   end Prune;

end Leander.Syntax;
