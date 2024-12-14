with Leander.Core.Kinds;
with Leander.Showable;

package Leander.Core.Tyvars is

   type Abstraction is interface
     and Kinds.Has_Kind
     and Showable.Abstraction;

   type Reference is not null access constant Abstraction'Class;
   type Tyvar_Array is array (Positive range <>) of Reference;

   function Name
     (This : Abstraction)
      return Name_Id
      is abstract;

   function Tyvar
     (Id : Name_Id;
      Kind : Kinds.Reference)
      return Reference;

   type Container_Abstraction is interface;

   function Contains
     (This  : Container_Abstraction;
      Tyvar : Reference)
      return Boolean
      is abstract;

end Leander.Core.Tyvars;
