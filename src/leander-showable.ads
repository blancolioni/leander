package Leander.Showable is

   type Abstraction is interface;

   function Show (This : Abstraction) return String is abstract;

end Leander.Showable;
