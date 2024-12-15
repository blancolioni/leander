private package Leander.Syntax.Expressions.Lets is

   subtype Parent is Leander.Syntax.Expressions.Instance;
   type Instance is new Parent with private;

   function Let
     (Location    : Leander.Source.Source_Location;
      Bindings    : Leander.Syntax.Bindings.Reference;
      Expr        : Reference)
      return Reference;

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference;

private

   type Instance is new Parent with
      record
         Bindings    : Leander.Syntax.Bindings.Reference;
         Expr        : Reference;
      end record;

   function Let
     (Location    : Leander.Source.Source_Location;
      Bindings    : Leander.Syntax.Bindings.Reference;
      Expr        : Reference)
      return Reference
   is (new Instance'(Location, Bindings, Expr));

end Leander.Syntax.Expressions.Lets;
