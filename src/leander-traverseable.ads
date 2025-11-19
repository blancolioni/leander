package Leander.Traverseable is

   type Abstraction is limited interface;

   procedure Traverse
     (This : not null access constant Abstraction;
      Process : not null access
        procedure (This : not null access constant Abstraction'Class))
   is abstract;

   procedure Update_Traverse
     (This    : not null access Abstraction;
      Process : not null access
        procedure (This : not null access Abstraction'Class))
   is null;

end Leander.Traverseable;
