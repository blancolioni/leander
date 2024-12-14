with Leander.Showable;

package Leander.Core.Kinds is

   type Abstraction is interface
     and Leander.Showable.Abstraction;

   type Reference is not null access constant Abstraction'Class;

   function KAp
     (This : Abstraction)
      return Reference
      is abstract;

   function Star return Reference;
   function KFun (Left, Right : Reference) return Reference;

   type Has_Kind is interface;

   function Kind (This : Has_Kind) return Reference is abstract;

end Leander.Core.Kinds;
