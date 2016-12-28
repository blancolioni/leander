package Leander is

   pragma Pure (Leander);

   type Show_Interface is interface;

   function Show (Item : Show_Interface) return String
                  is abstract;

end Leander;
