with Leander.Core.Types;
with Leander.Core.Tyvars;
with Leander.Maybes;
with Leander.Showable;

package Leander.Inference.Substitutions is

   type Abstraction is interface and Leander.Showable.Abstraction;
   type Reference is not null access constant Abstraction'Class;

   package Maybe_Types is
     new Leander.Maybes (Leander.Core.Types.Reference);

   function Lookup
     (This  : Abstraction;
      Tyvar : Core.Tyvars.Reference)
      return Maybe_Types.Maybe
      is abstract;

   function Merge
     (This  : not null access constant Abstraction;
      Other : not null access constant Abstraction'Class)
      return Reference
      is abstract;

   function Empty return Reference;
   function Substitute
     (Tyvar      : Leander.Core.Tyvars.Reference;
      Bound_Type : Leander.Core.Types.Reference)
      return Reference;

   function Apply
     (This : not null access constant Abstraction;
      To   : Core.Types.Reference)
      return Core.Types.Reference
      is abstract;

end Leander.Inference.Substitutions;
