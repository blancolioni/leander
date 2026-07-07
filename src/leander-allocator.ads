generic
   Name : String;
   type Instance (<>) is private;
   type Reference is access all Instance;
package Leander.Allocator is

   function Allocate (This : Instance) return Reference;
   procedure Protect (This : Reference);
   procedure Prune;

   --  Print allocation statistics for this instantiation (live and all-time
   --  allocation counts and byte totals) to standard output.
   procedure Report;

end Leander.Allocator;
