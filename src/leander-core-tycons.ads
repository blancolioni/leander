with Leander.Core.Kinds;
with Leander.Showable;

package Leander.Core.Tycons is

   type Abstraction is interface
     and Kinds.Has_Kind
     and Showable.Abstraction;

   type Reference is not null access constant Abstraction'Class;

   function Name
     (This : Abstraction)
      return Name_Id
      is abstract;

   function Tycon
     (Id   : Name_Id;
      Kind : Kinds.Reference)
      return Reference;

end Leander.Core.Tycons;
