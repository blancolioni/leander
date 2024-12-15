with Leander.Core.Substitutions;
with Leander.Core.Types;
with Leander.Core.Tyvars;

package Leander.Core.Schemes is

   type Abstraction is interface and Tyvars.Container_Abstraction;
   type Reference is access constant Abstraction'Class;

   function Fresh_Instance
     (This : Abstraction)
      return Types.Reference
     is abstract;

   function To_Scheme
     (T     : Types.Reference)
      return Reference;

   function Quantify
     (Vs    : Tyvars.Tyvar_Array;
      T     : Types.Reference)
      return Reference;

   function Apply
     (This  : Abstraction;
      Subst : Substitutions.Reference)
      return Reference
      is abstract;

end Leander.Core.Schemes;
