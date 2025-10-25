package Leander.Disposable is

   type Abstraction is limited interface;

   procedure Dispose (This : in out Abstraction) is abstract;

end Leander.Disposable;
