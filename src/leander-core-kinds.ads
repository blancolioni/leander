package Leander.Core.Kinds is

   type Kind is private;

   function Star return Kind;
   function Kind_Function (Left, Right : Kind) return Kind;

   function Is_Star (K : Kind) return Boolean;

   function Left_Kind (K : Kind) return Kind
     with Pre => not Is_Star (K);

   function Right_Kind (K : Kind) return Kind
     with Pre => not Is_Star (K);

   function Show (K : Kind) return String;

   type Has_Kind is interface;

   function Get_Kind (This : Has_Kind) return Kind is abstract;

private

   type Kind is new Natural;

   function Star return Kind is (0);

   function Is_Star (K : Kind) return Boolean is (K = 0);

end Leander.Core.Kinds;
