limited with Leander.Inference.Substitutions;
with Leander.Core.Tyvars;

package Leander.Inference.Types is

   type Abstraction is interface;
   type Reference is not null access constant Abstraction'Class;

   function Apply
     (This : not null access constant Abstraction;
      Sub  : not null access constant Substitutions.Reference)
      return Reference
      is abstract;

   function Tvs
     (This : Abstraction)
      return Leander.Core.Tyvars.Tyvar_Array
      is abstract;

end Leander.Inference.Types;
