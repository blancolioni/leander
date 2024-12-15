limited with Leander.Core.Types;
with Leander.Core.Tyvars;
with Leander.Maybes;
with Leander.Showable;

package Leander.Core.Substitutions is

   type Abstraction is interface and Leander.Showable.Abstraction;
   type Reference is not null access constant Abstraction'Class;

   type Type_Reference is access constant Types.Abstraction'Class;
   type Type_Reference_Array is array (Positive range <>) of Type_Reference;

   package Maybe_Result is
     new Leander.Maybes (Type_Reference);

   function Lookup
     (This  : Abstraction;
      Tyvar : Core.Tyvars.Reference)
      return Maybe_Result.Maybe
      is abstract;

   function Merge
     (This  : not null access constant Abstraction;
      Other : not null access constant Abstraction'Class)
      return Reference
      is abstract;

   function Empty return Reference;
   function Substitute
     (Tyvar   : Leander.Core.Tyvars.Reference;
      Element : not null access constant Types.Abstraction'Class)
      return Reference;

   function Substitute
     (Tyvars   : Leander.Core.Tyvars.Tyvar_Array;
      Elements : Type_Reference_Array)
      return Reference
     with Pre => Tyvars'Length = Elements'Length;

end Leander.Core.Substitutions;
