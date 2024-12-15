with Leander.Core.Schemes;
with Leander.Core.Substitutions;
with Leander.Core.Tyvars;
with Leander.Maybes;
with Leander.Showable;

package Leander.Core.Assumptions is

   type Abstraction is interface
     and Tyvars.Container_Abstraction
     and Showable.Abstraction;
   type Reference is not null access constant Abstraction'Class;

   package Maybe_Schemes is
     new Leander.Maybes (Leander.Core.Schemes.Reference);

   function Find
     (This : Abstraction;
      Id   : Name_Id)
      return Maybe_Schemes.Maybe
      is abstract;

   function Empty return Reference;

   function Assumption
     (Id     : Name_Id;
      Scheme : Schemes.Reference)
      return Reference;

   function Append
     (This    : Abstraction;
      Id      : Name_Id;
      Scheme  : Schemes.Reference)
      return Reference
      is abstract;

   function Append
     (This    : Abstraction;
      That    : not null access constant Abstraction'Class)
      return Reference
      is abstract;

   function Prepend
     (This    : Abstraction;
      Id      : Name_Id;
      Scheme  : Schemes.Reference)
      return Reference
      is abstract;

   function Prepend
     (This    : Abstraction;
      That    : not null access constant Abstraction'Class)
      return Reference
      is abstract;

   function Join
     (Left    : not null access constant Abstraction;
      Right   : not null access constant Abstraction'Class)
      return Reference
      is abstract;

   function Apply
     (This  : Abstraction;
      Subst : Substitutions.Reference)
      return Reference
      is abstract;

end Leander.Core.Assumptions;
