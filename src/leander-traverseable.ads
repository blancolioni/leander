package Leander.Traverseable is

   type Abstraction is limited interface;

   procedure Traverse
     (This : not null access constant Abstraction;
      Process : not null access
        procedure (This : not null access constant Abstraction'Class))
   is abstract;

end Leander.Traverseable;
