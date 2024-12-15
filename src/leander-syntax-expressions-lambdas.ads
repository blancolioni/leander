private package Leander.Syntax.Expressions.Lambdas is

   subtype Parent is Leander.Syntax.Expressions.Instance;
   type Instance is new Parent with private;

   function Lambda
     (Location    : Leander.Source.Source_Location;
      Pat         : Leander.Syntax.Patterns.Reference;
      Expr        : Reference)
      return Reference;

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference;

private

   type Instance is new Parent with
      record
         Pat  : Leander.Syntax.Patterns.Reference;
         Expr : Reference;
      end record;

   function Lambda
     (Location    : Leander.Source.Source_Location;
      Pat         : Leander.Syntax.Patterns.Reference;
      Expr        : Reference)
      return Reference
   is (new Instance'(Location, Pat, Expr));

end Leander.Syntax.Expressions.Lambdas;
