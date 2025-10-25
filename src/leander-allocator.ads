generic
   Name : String;
   type Instance (<>) is private;
   type Reference is access all Instance;
package Leander.Allocator is

   function Allocate (This : Instance) return Reference;
   procedure Protect (This : Reference);
   procedure Prune;

end Leander.Allocator;
