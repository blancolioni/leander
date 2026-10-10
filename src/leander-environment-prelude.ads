package Leander.Environment.Prelude is

   Max_Tuple_Arity : constant := 15;
   --  The largest tuple type and constructor Create registers.  Haskell
   --  2010 (section 6.1.4) requires at least 15.

   function Create return Reference;

end Leander.Environment.Prelude;
