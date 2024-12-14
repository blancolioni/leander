with Leander.Core.Types;
with Leander.Showable;

package Leander.Core.Literals is

   type Abstraction is interface
     and Leander.Showable.Abstraction;

   type Reference is access constant Abstraction'Class;

   function Literal_Type
     (This : Abstraction)
      return Leander.Core.Types.Reference
      is abstract;

   function Integer_Literal
     (Image : String)
      return Reference;

   function Character_Literal
     (Code : Natural)
      return Reference;

   function Float_Literal
     (Image : String)
      return Reference;

   function String_Literal
     (S : String)
      return Reference;

end Leander.Core.Literals;
