with Leander.Core.Types;
with Leander.Maybes;

package Leander.Core.Assumptions is

   type Abstraction is interface;
   type Reference is not null access constant Abstraction'Class;

   package Maybe_Types is
     new Leander.Maybes (Leander.Core.Types.Reference);

   function Find
     (This : Abstraction;
      Id   : Name_Id)
      return Maybe_Types.Maybe
      is abstract;

   function Empty return Reference;

   function Assumption
     (Id : Name_Id;
      T  : Types.Reference)
      return Reference;

   function Append
     (This    : Abstraction;
      Id      : Name_Id;
      Binding : Types.Reference)
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
      Binding : Types.Reference)
      return Reference
      is abstract;

   function Prepend
     (This    : Abstraction;
      That    : not null access constant Abstraction'Class)
      return Reference
      is abstract;

end Leander.Core.Assumptions;
