package Leander.Core.Bindings.Dependencies is

   type Component_Array is array (Positive range <>) of Positive;

   function Components
     (Bs      : Reference_Array;
      Depends : access function (From, To : Positive) return Boolean := null)
      return Component_Array
     with Post => Components'Result'First = Bs'First
     and then Components'Result'Last = Bs'Last;
   --  The strongly connected components of the dependency graph over Bs:
   --  Result (I) is the component of Bs (I).  Bs (I) depends on Bs (J) when
   --  Depends (I, J), or by default when Bs (I) refers to Bs (J)'s name.
   --  Components are numbered from 1 so that everything a component depends
   --  on has a lower number, which is the order bindings must be inferred
   --  in (Jones 1999, section 11.6.3), and the reverse of the order a let
   --  must nest them in.

   function Component_Count (Components : Component_Array) return Natural;

end Leander.Core.Bindings.Dependencies;
