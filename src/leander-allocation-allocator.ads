generic
   type Object is new Abstraction with private;
   type Reference is access all Object'Class;
package Leander.Allocation.Allocator is

   function Allocate return Reference;
   procedure Deallocate (This : Reference);

end Leander.Allocation.Allocator;
